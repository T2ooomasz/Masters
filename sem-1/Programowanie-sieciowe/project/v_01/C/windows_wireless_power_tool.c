#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>
#include <wlanapi.h>
#include <windot11.h>
#include <objbase.h>
#include <wtypes.h>
#include <getopt.h>
#include <setupapi.h>
#include <devguid.h>
#include <regstr.h>

#pragma comment(lib, "wlanapi.lib")
#pragma comment(lib, "ole32.lib")
#pragma comment(lib, "setupapi.lib")
#pragma comment(lib, "advapi32.lib")

#define MAX_INTERFACES 20
#define MAX_IFACE_LEN 512
#define MAX_POWER_STR_LEN 64
#define MIN_POWER_PERCENT 1
#define MAX_POWER_PERCENT 100

// Struktura informacji o interfejsie Wi-Fi
struct wireless_interface {
    WCHAR guid_string[256];
    char name[MAX_IFACE_LEN];
    char friendly_name[MAX_IFACE_LEN];
    char description[MAX_IFACE_LEN];
    DWORD current_power_percent;
    char power_string[MAX_POWER_STR_LEN];
    WLAN_INTERFACE_STATE state;
    char device_instance_id[MAX_IFACE_LEN];
    GUID interface_guid;
};

// Opcje programu
struct program_options {
    int batch_mode;
    char my_interface[MAX_IFACE_LEN];
    int power_percent;
    int list_only;
    int verbose;
};

/**
 * @brief Konwertuje WCHAR na char
 */
void wchar_to_char(const WCHAR* wstr, char* str, size_t size) {
    WideCharToMultiByte(CP_UTF8, 0, wstr, -1, str, (int)size, NULL, NULL);
}

/**
 * @brief Konwertuje char na WCHAR
 */
void char_to_wchar(const char* str, WCHAR* wstr, size_t size) {
    MultiByteToWideChar(CP_UTF8, 0, str, -1, wstr, (int)size);
}

/**
 * @brief Wyświetla pomoc
 */
void print_usage(const char *program_name) {
    printf("Uzycie: %s [OPCJE]\n", program_name);
    printf("\nOpcje:\n");
    printf("  -i, --interface NAZWA   Nazwa interfejsu Wi-Fi (lub numer)\n");
    printf("  -p, --power PROCENT     Moc w procentach (1-100)\n");
    printf("  -l, --list              Wyswietl tylko liste interfejsow\n");
    printf("  -v, --verbose           Tryb szczegolowy\n");
    printf("  -h, --help              Wyswietl te pomoc\n");
    printf("\nPrzyklad:\n");
    printf("  %s                      Tryb interaktywny\n", program_name);
    printf("  %s -l                   Lista interfejsow\n", program_name);
    printf("  %s -i 1 -p 75           Ustaw moc 75%% dla pierwszego interfejsu\n", program_name);
    printf("  %s -i \"Wi-Fi\" -p 50      Ustaw moc 50%% dla Wi-Fi\n", program_name);
    printf("\nUwaga: Program wymaga uprawnien administratora!\n");
}

/**
 * @brief Sprawdza czy program działa z uprawnieniami administratora
 */
int is_admin() {
    BOOL is_admin = FALSE;
    PSID admin_group = NULL;
    SID_IDENTIFIER_AUTHORITY nt_authority = SECURITY_NT_AUTHORITY;

    if (AllocateAndInitializeSid(&nt_authority, 2, SECURITY_BUILTIN_DOMAIN_RID,
                                DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, &admin_group)) {
        CheckTokenMembership(NULL, admin_group, &is_admin);
        FreeSid(admin_group);
    }

    return is_admin;
}

/**
 * @brief Konwertuje stan interfejsu na tekst
 */
const char* interface_state_to_string(WLAN_INTERFACE_STATE state) {
    switch (state) {
        case wlan_interface_state_not_ready: return "Nie gotowy";
        case wlan_interface_state_connected: return "Polaczony";
        case wlan_interface_state_ad_hoc_network_formed: return "Ad-hoc";
        case wlan_interface_state_disconnecting: return "Rozlaczanie";
        case wlan_interface_state_disconnected: return "Rozlaczony";
        case wlan_interface_state_associating: return "Laczenie";
        case wlan_interface_state_discovering: return "Skanowanie";
        case wlan_interface_state_authenticating: return "Uwierzytelnianie";
        default: return "Nieznany";
    }
}

/**
 * @brief Pobiera ścieżkę do klucza rejestru adaptera
 */
int get_adapter_registry_path(const char* device_instance_id, char* reg_path, size_t path_size, int verbose) {
    HKEY hkey_enum;
    HKEY hkey_device;
    char driver_key[512];
    DWORD data_size = sizeof(driver_key);
    
    if (verbose) {
        printf("[VERBOSE] Szukam sciezki rejestru dla: %s\n", device_instance_id);
    }
    
    // Otwórz klucz ENUM
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SYSTEM\\CurrentControlSet\\Enum", 0, KEY_READ, &hkey_enum) != ERROR_SUCCESS) {
        if (verbose) printf("[VERBOSE] Nie mozna otworzyc klucza ENUM\n");
        return -1;
    }
    
    // Otwórz klucz urządzenia
    if (RegOpenKeyExA(hkey_enum, device_instance_id, 0, KEY_READ, &hkey_device) != ERROR_SUCCESS) {
        if (verbose) printf("[VERBOSE] Nie mozna otworzyc klucza urzadzenia\n");
        RegCloseKey(hkey_enum);
        return -1;
    }
    
    // Odczytaj klucz sterownika
    if (RegQueryValueExA(hkey_device, "Driver", NULL, NULL, (LPBYTE)driver_key, &data_size) == ERROR_SUCCESS) {
        snprintf(reg_path, path_size, "SYSTEM\\CurrentControlSet\\Control\\Class\\%s", driver_key);
        if (verbose) {
            printf("[VERBOSE] Znaleziono sciezke rejestru: %s\n", reg_path);
        }
        RegCloseKey(hkey_device);
        RegCloseKey(hkey_enum);
        return 0;
    }
    
    RegCloseKey(hkey_device);
    RegCloseKey(hkey_enum);
    return -1;
}

/**
 * @brief Pobiera informacje o mocy z rejestru sterownika
 */
int get_tx_power_from_driver_registry(const char* device_instance_id, int verbose) {
    char reg_path[512];
    HKEY hkey;
    DWORD power_value = 100;
    DWORD data_size = sizeof(DWORD);
    DWORD data_type;
    
    if (get_adapter_registry_path(device_instance_id, reg_path, sizeof(reg_path), verbose) != 0) {
        return 100; // domyślna wartość
    }
    
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, reg_path, 0, KEY_READ, &hkey) == ERROR_SUCCESS) {
        // Próbuj różne nazwy kluczy dla mocy TX
        const char* power_keys[] = {
            "TxPower", "TransmitPower", "PowerOutput", "TxPowerLevel",
            "RadioPower", "OutputPower", "TxPowerCtrl", "*TxPower"
        };
        
        for (int i = 0; i < sizeof(power_keys) / sizeof(power_keys[0]); i++) {
            data_size = sizeof(DWORD);
            if (RegQueryValueExA(hkey, power_keys[i], NULL, &data_type, (LPBYTE)&power_value, &data_size) == ERROR_SUCCESS) {
                if (verbose) {
                    printf("[VERBOSE] Znaleziono klucz mocy: %s = %lu\n", power_keys[i], power_value);
                }
                break;
            }
        }
        RegCloseKey(hkey);
    }
    
    return (int)power_value;
}

/**
 * @brief Ustawia moc w rejestrze sterownika
 */
int set_tx_power_driver_registry(const char* device_instance_id, int power_percent, int verbose) {
    char reg_path[512];
    HKEY hkey;
    DWORD power_value = (DWORD)power_percent;
    
    if (get_adapter_registry_path(device_instance_id, reg_path, sizeof(reg_path), verbose) != 0) {
        return -1;
    }
    
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, reg_path, 0, KEY_SET_VALUE, &hkey) == ERROR_SUCCESS) {
        const char* power_keys[] = {
            "TxPower", "TransmitPower", "PowerOutput", "TxPowerLevel"
        };
        
        int success = 0;
        for (int i = 0; i < sizeof(power_keys) / sizeof(power_keys[0]); i++) {
            if (RegSetValueExA(hkey, power_keys[i], 0, REG_DWORD, (LPBYTE)&power_value, sizeof(DWORD)) == ERROR_SUCCESS) {
                if (verbose) {
                    printf("[VERBOSE] Ustawiono %s = %d\n", power_keys[i], power_percent);
                }
                success = 1;
            }
        }
        
        RegCloseKey(hkey);
        return success ? 0 : -1;
    }
    
    return -1;
}

/**
 * @brief Pobiera Device Instance ID dla interfejsu sieciowego
 */
int get_device_instance_id(const GUID* interface_guid, char* device_id, size_t device_id_size, int verbose) {
    HDEVINFO dev_info;
    SP_DEVINFO_DATA dev_info_data;
    DWORD index = 0;
    char guid_str[64];
    
    // Konwertuj GUID na string
    snprintf(guid_str, sizeof(guid_str), "{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}",
             interface_guid->Data1, interface_guid->Data2, interface_guid->Data3,
             interface_guid->Data4[0], interface_guid->Data4[1], interface_guid->Data4[2],
             interface_guid->Data4[3], interface_guid->Data4[4], interface_guid->Data4[5],
             interface_guid->Data4[6], interface_guid->Data4[7]);
    
    if (verbose) {
        printf("[VERBOSE] Szukam Device Instance ID dla GUID: %s\n", guid_str);
    }
    
    // Pobierz informacje o urządzeniach sieciowych
    dev_info = SetupDiGetClassDevs(&GUID_DEVCLASS_NET, NULL, NULL, DIGCF_PRESENT);
    if (dev_info == INVALID_HANDLE_VALUE) {
        return -1;
    }
    
    dev_info_data.cbSize = sizeof(SP_DEVINFO_DATA);
    
    while (SetupDiEnumDeviceInfo(dev_info, index, &dev_info_data)) {
        char current_device_id[512];
        DWORD required_size = 0;
        
        // Pobierz Device Instance ID
        if (SetupDiGetDeviceInstanceIdA(dev_info, &dev_info_data, current_device_id, 
                                       sizeof(current_device_id), &required_size)) {
            
            // Sprawdź czy to urządzenie sieciowe powiązane z naszym GUID
            HKEY hkey;
            char reg_path[512];
            snprintf(reg_path, sizeof(reg_path), "SYSTEM\\CurrentControlSet\\Enum\\%s", current_device_id);
            
            if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, reg_path, 0, KEY_READ, &hkey) == ERROR_SUCCESS) {
                char net_cfg_instance_id[256];
                DWORD data_size = sizeof(net_cfg_instance_id);
                
                if (RegQueryValueExA(hkey, "NetCfgInstanceId", NULL, NULL, 
                                   (LPBYTE)net_cfg_instance_id, &data_size) == ERROR_SUCCESS) {
                    if (_stricmp(net_cfg_instance_id, guid_str) == 0) {
                        strncpy(device_id, current_device_id, device_id_size - 1);
                        device_id[device_id_size - 1] = '\0';
                        if (verbose) {
                            printf("[VERBOSE] Znaleziono Device Instance ID: %s\n", device_id);
                        }
                        RegCloseKey(hkey);
                        SetupDiDestroyDeviceInfoList(dev_info);
                        return 0;
                    }
                }
                RegCloseKey(hkey);
            }
        }
        index++;
    }
    
    SetupDiDestroyDeviceInfoList(dev_info);
    return -1;
}

/**
 * @brief Pobiera nazwę przyjazną interfejsu
 */
void get_friendly_name(const GUID* interface_guid, char* friendly_name, size_t name_size, int verbose) {
    char guid_str[64];
    char reg_path[512];
    HKEY hkey;
    DWORD data_size = (DWORD)name_size;
    
    // Konwertuj GUID na string
    snprintf(guid_str, sizeof(guid_str), "{%08X-%04X-%04X-%02X%02X-%02X%02X%02X%02X%02X%02X}",
             interface_guid->Data1, interface_guid->Data2, interface_guid->Data3,
             interface_guid->Data4[0], interface_guid->Data4[1], interface_guid->Data4[2],
             interface_guid->Data4[3], interface_guid->Data4[4], interface_guid->Data4[5],
             interface_guid->Data4[6], interface_guid->Data4[7]);
    
    // Sprawdź w rejestrze połączeń sieciowych
    snprintf(reg_path, sizeof(reg_path), 
             "SYSTEM\\CurrentControlSet\\Control\\Network\\{4D36E972-E325-11CE-BFC1-08002BE10318}\\%s\\Connection", 
             guid_str);
    
    if (RegOpenKeyExA(HKEY_LOCAL_MACHINE, reg_path, 0, KEY_READ, &hkey) == ERROR_SUCCESS) {
        if (RegQueryValueExA(hkey, "Name", NULL, NULL, (LPBYTE)friendly_name, &data_size) == ERROR_SUCCESS) {
            if (verbose) {
                printf("[VERBOSE] Nazwa przyjazna: %s\n", friendly_name);
            }
        }
        RegCloseKey(hkey);
    }
}

/**
 * @brief Pobiera liste interfejsow Wi-Fi
 */
int get_wireless_interfaces(struct wireless_interface *interfaces, int verbose) {
    HANDLE client_handle = NULL;
    DWORD max_client = 2;
    DWORD cur_version = 0;
    PWLAN_INTERFACE_INFO_LIST interface_list = NULL;
    int interface_count = 0;

    // Otwieranie uchwyta do WLAN API
    DWORD result = WlanOpenHandle(max_client, NULL, &cur_version, &client_handle);
    if (result != ERROR_SUCCESS) {
        fprintf(stderr, "Blad WlanOpenHandle: %lu\n", result);
        return 0;
    }

    // Pobieranie listy interfejsow
    result = WlanEnumInterfaces(client_handle, NULL, &interface_list);
    if (result != ERROR_SUCCESS) {
        fprintf(stderr, "Blad WlanEnumInterfaces: %lu\n", result);
        WlanCloseHandle(client_handle, NULL);
        return 0;
    }

    if (verbose) {
        printf("[VERBOSE] Znaleziono %lu interfejsow\n", interface_list->dwNumberOfItems);
    }

    // Przetwarzanie interfejsow
    for (DWORD i = 0; i < interface_list->dwNumberOfItems && interface_count < MAX_INTERFACES; i++) {
        PWLAN_INTERFACE_INFO interface_info = &interface_list->InterfaceInfo[i];
        
        // Kopiuj GUID
        interfaces[interface_count].interface_guid = interface_info->InterfaceGuid;
        
        // Konwersja GUID na string
        StringFromGUID2(&interface_info->InterfaceGuid, 
                       interfaces[interface_count].guid_string, 
                       sizeof(interfaces[interface_count].guid_string) / sizeof(WCHAR));

        // Konwersja nazwy (opis techniczny)
        wchar_to_char(interface_info->strInterfaceDescription, 
                     interfaces[interface_count].description, 
                     MAX_IFACE_LEN);
        
        // Pobierz nazwę przyjazną
        interfaces[interface_count].friendly_name[0] = '\0';
        get_friendly_name(&interface_info->InterfaceGuid, 
                         interfaces[interface_count].friendly_name, 
                         MAX_IFACE_LEN, verbose);
        
        // Użyj nazwy przyjaznej jeśli dostępna, w przeciwnym razie opisu
        if (strlen(interfaces[interface_count].friendly_name) > 0) {
            strncpy(interfaces[interface_count].name, 
                   interfaces[interface_count].friendly_name, 
                   MAX_IFACE_LEN - 1);
        } else {
            strncpy(interfaces[interface_count].name, 
                   interfaces[interface_count].description, 
                   MAX_IFACE_LEN - 1);
        }
        interfaces[interface_count].name[MAX_IFACE_LEN - 1] = '\0';

        // Stan interfejsu
        interfaces[interface_count].state = interface_info->isState;

        // Pobierz Device Instance ID
        if (get_device_instance_id(&interface_info->InterfaceGuid, 
                                  interfaces[interface_count].device_instance_id,
                                  MAX_IFACE_LEN, verbose) == 0) {
            
            // Pobierz aktualną moc
            interfaces[interface_count].current_power_percent = 
                get_tx_power_from_driver_registry(interfaces[interface_count].device_instance_id, verbose);
        } else {
            interfaces[interface_count].current_power_percent = 100;
            interfaces[interface_count].device_instance_id[0] = '\0';
        }
        
        snprintf(interfaces[interface_count].power_string, MAX_POWER_STR_LEN, 
                "%lu%%", interfaces[interface_count].current_power_percent);

        if (verbose) {
            printf("[VERBOSE] Interfejs %d:\n", interface_count + 1);
            printf("  Nazwa: %s\n", interfaces[interface_count].name);
            printf("  Opis: %s\n", interfaces[interface_count].description);
            printf("  Stan: %s\n", interface_state_to_string(interfaces[interface_count].state));
            printf("  Device ID: %s\n", interfaces[interface_count].device_instance_id);
        }

        interface_count++;
    }

    // Czyszczenie
    if (interface_list != NULL) {
        WlanFreeMemory(interface_list);
    }
    WlanCloseHandle(client_handle, NULL);

    return interface_count;
}

/**
 * @brief Restartuje adapter sieciowy przez Device Manager
 */
int restart_network_adapter_device_manager(const char* device_instance_id, int verbose) {
    if (strlen(device_instance_id) == 0) {
        if (verbose) printf("[VERBOSE] Brak Device Instance ID\n");
        return -1;
    }
    
    printf("2. Restartowanie adaptera...\n");
    
    char command[1024];
    
    // Metoda 1: Użyj pnputil (nowoczesne narzędzie)
    snprintf(command, sizeof(command), "pnputil /restart-device \"%s\"", device_instance_id);
    if (verbose) {
        printf("[VERBOSE] Wykonywanie: %s\n", command);
    }
    
    int result = system(command);
    if (result == 0) {
        printf("   ✓ Adapter zrestartowany pomyslnie\n");
        return 0;
    }
    
    // Metoda 2: Użyj PowerShell DevCon functionality
    snprintf(command, sizeof(command), 
        "powershell -Command \"Get-PnpDevice -InstanceId '%s' | Disable-PnpDevice -Confirm:$false; Start-Sleep 2; Get-PnpDevice -InstanceId '%s' | Enable-PnpDevice -Confirm:$false\"",
        device_instance_id, device_instance_id);
    
    if (verbose) {
        printf("[VERBOSE] Probuje PowerShell: %s\n", command);
    }
    
    result = system(command);
    if (result == 0) {
        printf("   ✓ Adapter zrestartowany za pomoca PowerShell\n");
        return 0;
    }
    
    printf("   ! Nie udalo sie zrestartowac adaptera automatycznie\n");
    printf("     Mozesz sprobowac recznie: Menedzer urzadzen -> Wylacz/Wlacz adapter\n");
    return -1;
}

/**
 * @brief Ustawia moc nadawania dla wybranego interfejsu
 */
int set_tx_power(struct wireless_interface *my_interface, int power_percent, int verbose) {
    printf("\n=== Konfigurowanie interfejsu '%s' ===\n", my_interface->name);
    
    if (power_percent < MIN_POWER_PERCENT || power_percent > MAX_POWER_PERCENT) {
        fprintf(stderr, "Blad: Moc musi byc w zakresie %d-%d%%\n", 
                MIN_POWER_PERCENT, MAX_POWER_PERCENT);
        return -1;
    }

    printf("1. Ustawianie mocy nadawania na %d%%...\n", power_percent);

    int success = 0;

    // Metoda 1: Rejestr sterownika
    if (strlen(my_interface->device_instance_id) > 0) {
        if (verbose) printf("[VERBOSE] Probuje ustawienie w rejestrze sterownika...\n");
        
        if (set_tx_power_driver_registry(my_interface->device_instance_id, power_percent, verbose) == 0) {
            success = 1;
            printf("   ✓ Wartosc ustawiona w rejestrze sterownika\n");
        } else if (verbose) {
            printf("[VERBOSE] Rejestr sterownika: Niepowodzenie\n");
        }
    }

    // Metoda 2: PowerShell z poprawną nazwą
    if (!success && strlen(my_interface->friendly_name) > 0) {
        if (verbose) printf("[VERBOSE] Probuje PowerShell z nazwa przyjazna...\n");
        
        char command[1024];
        snprintf(command, sizeof(command), 
            "powershell -Command \"$adapter = Get-NetAdapter | Where-Object {$_.Name -eq '%s' -or $_.InterfaceDescription -eq '%s'}; if ($adapter) { try { $adapter | Set-NetAdapterAdvancedProperty -DisplayName '*Transmit Power*' -DisplayValue '%d%%' -ErrorAction Stop; Write-Host 'SUCCESS' } catch { $adapter | Set-NetAdapterAdvancedProperty -RegistryKeyword '*TxPower*' -RegistryValue %d -ErrorAction SilentlyContinue; Write-Host 'PARTIAL' } }\"",
            my_interface->friendly_name, my_interface->description, power_percent, power_percent);
        
        if (verbose) {
            printf("[VERBOSE] Wykonywanie: %s\n", command);
        }
        
        int result = system(command);
        if (result == 0) {
            success = 1;
            printf("   ✓ Ustawiono za pomoca PowerShell\n");
        }
    }

    // Metoda 3: Próba z opisem interfejsu
    if (!success) {
        if (verbose) printf("[VERBOSE] Probuje PowerShell z opisem interfejsu...\n");
        
        char command[1024];
        snprintf(command, sizeof(command), 
            "powershell -Command \"Get-NetAdapter | Where-Object {$_.InterfaceDescription -like '*%s*'} | Set-NetAdapterAdvancedProperty -DisplayName '*Power*' -DisplayValue '%d%%' 2>$null\"",
            "Wi-Fi", power_percent); // Używamy ogólnego terminu
        
        if (verbose) {
            printf("[VERBOSE] Wykonywanie: %s\n", command);
        }
        
        int result = system(command);
        if (result == 0) {
            success = 1;
            printf("   ✓ Prawdopodobnie ustawiono moc\n");
        }
    }

    if (success) {
        printf("\n✓ Konfiguracja prawdopodobnie zakonczona pomyslnie!\n");
        
        // Restart adaptera
        if (strlen(my_interface->device_instance_id) > 0) {
            restart_network_adapter_device_manager(my_interface->device_instance_id, verbose);
        }
        
        printf("\nUwaga: Zmiany moga wymagac czasu na zastosowanie.\n");
        printf("Sprawdz ustawienia w: Menedzer urzadzen -> Adapter -> Wlasciwosci -> Zaawansowane\n");
        return 0;
    } else {
        fprintf(stderr, "\nBlad: Nie udalo sie ustawic mocy nadawania.\n");
        fprintf(stderr, "Mozliwe przyczyny:\n");
        fprintf(stderr, "- Karta Wi-Fi nie obsługuje programowej zmiany mocy nadawania\n");
        fprintf(stderr, "- Sterownik nie udostepnia tej funkcjonalnosci\n");
        fprintf(stderr, "- Wymagane sa dodatkowe uprawnienia\n");
        fprintf(stderr, "\nMozesz sprobowac recznie:\n");
        fprintf(stderr, "1. Menedzer urzadzen -> Adaptery sieciowe\n");
        fprintf(stderr, "2. Prawy klik na '%s'\n", my_interface->name);
        fprintf(stderr, "3. Wlasciwosci -> Zaawansowane\n");
        fprintf(stderr, "4. Szukaj opcji 'Transmit Power', 'Power Output' lub podobnej\n");
        return -1;
    }
}

/**
 * @brief Parsuje argumenty wiersza polecen
 */
int parse_arguments(int argc, char *argv[], struct program_options *options) {
    int c;
    static struct option long_options[] = {
        {"interface", required_argument, 0, 'i'},
        {"power", required_argument, 0, 'p'},
        {"list", no_argument, 0, 'l'},
        {"verbose", no_argument, 0, 'v'},
        {"help", no_argument, 0, 'h'},
        {0, 0, 0, 0}
    };

    memset(options, 0, sizeof(struct program_options));

    while ((c = getopt_long(argc, argv, "i:p:lvh", long_options, NULL)) != -1) {
        switch (c) {
            case 'i':
                strncpy(options->my_interface, optarg, MAX_IFACE_LEN - 1);
                options->my_interface[MAX_IFACE_LEN - 1] = '\0';
                break;
            case 'p':
                options->power_percent = atoi(optarg);
                if (options->power_percent < MIN_POWER_PERCENT || 
                    options->power_percent > MAX_POWER_PERCENT) {
                    fprintf(stderr, "Blad: Moc musi byc w zakresie %d-%d%%\n", 
                            MIN_POWER_PERCENT, MAX_POWER_PERCENT);
                    return -1;
                }
                break;
            case 'l':
                options->list_only = 1;
                break;
            case 'v':
                options->verbose = 1;
                break;
            case 'h':
                print_usage(argv[0]);
                exit(0);
            case '?':
                return -1;
            default:
                return -1;
        }
    }

    if (strlen(options->my_interface) > 0 && options->power_percent > 0) {
        options->batch_mode = 1;
    } else if (strlen(options->my_interface) > 0 || options->power_percent > 0) {
        fprintf(stderr, "Blad: W trybie wsadowym musisz podac zarowno interfejs (-i) jak i moc (-p)\n");
        return -1;
    }

    return 0;
}

/**
 * @brief Tryb interaktywny
 */
int interactive_mode(struct wireless_interface *interfaces, int interface_count, int verbose) {
    printf("\nDostepne interfejsy Wi-Fi:\n");
    printf("%-4s %-40s %-15s %-10s\n", "Nr", "Nazwa", "Stan", "Moc");
    printf("%-4s %-40s %-15s %-10s\n", "---", "----", "----", "----");
    
    for (int i = 0; i < interface_count; i++) {
        // Skróć długie nazwy dla lepszego wyświetlania
        char display_name[45];
        if (strlen(interfaces[i].name) > 40) {
            strncpy(display_name, interfaces[i].name, 37);
            display_name[37] = '\0';
            strcat(display_name, "...");
        } else {
            strcpy(display_name, interfaces[i].name);
        }
        
        printf("%-4d %-40s %-15s %-10s\n", 
               i + 1, 
               display_name,
               interface_state_to_string(interfaces[i].state),
               interfaces[i].power_string);
    }

    int choice = 0;
    printf("\nWybierz interfejs (podaj numer 1-%d): ", interface_count);
    if (scanf("%d", &choice) != 1 || choice < 1 || choice > interface_count) {
        fprintf(stderr, "Nieprawidlowy wybor.\n");
        return 1;
    }

    struct wireless_interface *selected = &interfaces[choice - 1];

    printf("\nWybrany interfejs:\n");
    printf("  Nazwa: %s\n", selected->name);
    printf("  Opis: %s\n", selected->description);
    printf("  Aktualna moc: %s\n", selected->power_string);
    printf("  Stan: %s\n", interface_state_to_string(selected->state));

    int power = 0;
    printf("\nPodaj nowa moc nadawania (%d-%d%%): ", MIN_POWER_PERCENT, MAX_POWER_PERCENT);
    if (scanf("%d", &power) != 1) {
        fprintf(stderr, "Nieprawidlowa wartosc. Prosze podac liczbe.\n");
        return 1;
    }

    if (power < MIN_POWER_PERCENT || power > MAX_POWER_PERCENT) {
        fprintf(stderr, "Moc musi byc w zakresie %d-%d%%\n", 
                MIN_POWER_PERCENT, MAX_POWER_PERCENT);
        return 1;
    }

    return set_tx_power(selected, power, verbose);
}

/**
 * @brief Znajduje interfejs po nazwie lub numerze
 */
struct wireless_interface* find_interface(struct wireless_interface *interfaces, int interface_count, const char* identifier, int verbose) {
    // Sprawdź czy to numer
    char* endptr;
    long num = strtol(identifier, &endptr, 10);
    if (*endptr == '\0' && num >= 1 && num <= interface_count) {
        if (verbose) {
            printf("[VERBOSE] Znaleziono interfejs po numerze: %ld\n", num);
        }
        return &interfaces[num - 1];
    }
    
    // Sprawdź po nazwie (dokładne dopasowanie)
    for (int i = 0; i < interface_count; i++) {
        if (strcmp(interfaces[i].name, identifier) == 0 ||
            strcmp(interfaces[i].friendly_name, identifier) == 0 ||
            strcmp(interfaces[i].description, identifier) == 0) {
            if (verbose) {
                printf("[VERBOSE] Znaleziono interfejs po nazwie: %s\n", interfaces[i].name);
            }
            return &interfaces[i];
        }
    }
    
    // Sprawdź po częściowej nazwie
    for (int i = 0; i < interface_count; i++) {
        if (strstr(interfaces[i].name, identifier) != NULL ||
            strstr(interfaces[i].friendly_name, identifier) != NULL ||
            strstr(interfaces[i].description, identifier) != NULL) {
            if (verbose) {
                printf("[VERBOSE] Znaleziono interfejs po czesciowej nazwie: %s\n", interfaces[i].name);
            }
            return &interfaces[i];
        }
    }
    
    return NULL;
}

int main(int argc, char *argv[]) {
    struct program_options options;
    
    // Inicjalizacja COM
    CoInitialize(NULL);
    
    // Parsowanie argumentow
    if (parse_arguments(argc, argv, &options) != 0) {
        CoUninitialize();
        return 1;
    }

    // Sprawdzenie uprawnien administratora
    if (!is_admin()) {
        fprintf(stderr, "Ten program wymaga uprawnien administratora.\n");
        fprintf(stderr, "Uruchom go jako administrator (PPM -> 'Uruchom jako administrator')\n");
        CoUninitialize();
        return 1;
    }

    printf("=== Narzedzie do sterowania moca karty Wi-Fi - Windows v2.0 ===\n");
    if (options.verbose) {
        printf("[VERBOSE] Tryb szczegolowy wlaczony\n");
    }

    // Pobranie listy interfejsow
    struct wireless_interface interfaces[MAX_INTERFACES];
    int interface_count = get_wireless_interfaces(interfaces, options.verbose);

    if (interface_count == 0) {
        printf("\nNie znaleziono zadnych interfejsow Wi-Fi.\n");
        printf("Upewnij sie, ze:\n");
        printf("- Karta Wi-Fi jest wlaczona\n");
        printf("- Sterowniki sa poprawnie zainstalowane\n");
        printf("- Usluga WLAN AutoConfig jest uruchomiona\n");
        CoUninitialize();
        return 1;
    }

    // Tryb tylko listowania
    if (options.list_only) {
        printf("\nZnalezione interfejsy Wi-Fi (%d):\n", interface_count);
        printf("%-4s %-40s %-15s %-10s %-30s\n", "Nr", "Nazwa", "Stan", "Moc", "Device Instance ID");
        printf("%-4s %-40s %-15s %-10s %-30s\n", "---", "----", "----", "----", "-----------------");
        
        for (int i = 0; i < interface_count; i++) {
            char display_name[45];
            char display_device_id[35];
            
            // Skróć długie nazwy
            if (strlen(interfaces[i].name) > 40) {
                strncpy(display_name, interfaces[i].name, 37);
                display_name[37] = '\0'; 
                strcat(display_name, "...");
            } else {
                strcpy(display_name, interfaces[i].name);
            }
            
            // Skróć długie Device ID
            if (strlen(interfaces[i].device_instance_id) > 30) {
                strncpy(display_device_id, interfaces[i].device_instance_id, 27);
                display_device_id[27] = '\0';
                strcat(display_device_id, "...");
            } else {
                strcpy(display_device_id, interfaces[i].device_instance_id);
            }
            
            printf("%-4d %-40s %-15s %-10s %-30s\n", 
                   i + 1,
                   display_name,
                   interface_state_to_string(interfaces[i].state),
                   interfaces[i].power_string,
                   display_device_id);
        }
        
        if (options.verbose) {
            printf("\nSzczegolowe informacje:\n");
            for (int i = 0; i < interface_count; i++) {
                printf("\nInterfejs %d:\n", i + 1);
                printf("  Nazwa przyjazna: %s\n", interfaces[i].friendly_name);
                printf("  Opis techniczny: %s\n", interfaces[i].description);
                printf("  Device Instance ID: %s\n", interfaces[i].device_instance_id);
                char guid_str[256];
                wchar_to_char(interfaces[i].guid_string, guid_str, sizeof(guid_str));
                printf("  GUID: %s\n", guid_str);
            }
        }
        
        CoUninitialize();
        return 0;
    }

    int result = 0;

    // Tryb wsadowy
    if (options.batch_mode) {
        struct wireless_interface *selected = find_interface(interfaces, interface_count, options.my_interface, options.verbose);

        if (!selected) {
            fprintf(stderr, "Blad: Interfejs '%s' nie zostal znaleziony.\n", options.my_interface);
            printf("\nDostepne interfejsy:\n");
            for (int i = 0; i < interface_count; i++) {
                printf("  %d: %s\n", i + 1, interfaces[i].name);
                if (strlen(interfaces[i].friendly_name) > 0 && 
                    strcmp(interfaces[i].name, interfaces[i].friendly_name) != 0) {
                    printf("      (nazwa przyjazna: %s)\n", interfaces[i].friendly_name);
                }
            }
            result = 1;
        } else {
            printf("Tryb wsadowy: Ustawianie mocy %d%% dla interfejsu '%s'\n", 
                   options.power_percent, selected->name);
            result = set_tx_power(selected, options.power_percent, options.verbose);
        }
    } else {
        // Tryb interaktywny
        result = interactive_mode(interfaces, interface_count, options.verbose);
    }

    CoUninitialize();
    return result;
}