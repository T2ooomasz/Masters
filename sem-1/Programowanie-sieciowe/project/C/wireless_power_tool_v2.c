#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>

#define MAX_CMD_LEN 512
#define MAX_BUF_LEN 2048
#define MAX_INTERFACES 20
#define MAX_IFACE_LEN 32
#define MAX_POWER_STR_LEN 64
#define MIN_POWER_DBM 0
#define MAX_POWER_DBM 30

// Struktura do przechowywania informacji o interfejsie
struct wireless_interface {
    char name[MAX_IFACE_LEN];
    char tx_power[MAX_POWER_STR_LEN];
    int power_dbm;
    int min_power;
    int max_power;
    char driver[64];
};

// Opcje wiersza poleceń
struct program_options {
    int batch_mode;
    char interface[MAX_IFACE_LEN];
    int power;
    int list_only;
    int verbose;
};

/**
 * @brief Wyświetla pomoc dotyczącą użycia programu
 */
void print_usage(const char *program_name) {
    printf("Użycie: %s [OPCJE]\n", program_name);
    printf("\nOpcje:\n");
    printf("  -i, --interface NAZWA   Nazwa interfejsu (np. wlan0)\n");
    printf("  -p, --power WARTOŚĆ     Moc w dBm (0-%d)\n", MAX_POWER_DBM);
    printf("  -l, --list              Wyświetl tylko listę interfejsów\n");
    printf("  -v, --verbose           Tryb szczegółowy\n");
    printf("  -h, --help              Wyświetl tę pomoc\n");
    printf("\nPrzykłady:\n");
    printf("  %s                      Tryb interaktywny\n", program_name);
    printf("  %s -l                   Lista interfejsów\n", program_name);
    printf("  %s -i wlan0 -p 15       Ustaw moc 15 dBm dla wlan0\n", program_name);
}

/**
 * @brief Wykonuje polecenie systemowe i zapisuje jego wyjście do bufora
 */
int execute_command_get_output(const char *command, char *buffer, size_t buffer_size, int verbose) {
    if (verbose) {
        printf("[VERBOSE] Wykonywanie: %s\n", command);
    }
    
    FILE *fp = popen(command, "r");
    if (fp == NULL) {
        perror("Błąd podczas uruchamiania polecenia (popen)");
        return -1;
    }

    size_t bytes_read = fread(buffer, 1, buffer_size - 1, fp);
    buffer[bytes_read] = '\0';

    int exit_code = pclose(fp);
    if (exit_code == -1) {
        perror("Błąd podczas zamykania strumienia (pclose)");
        return -1;
    }
    
    if (verbose && WEXITSTATUS(exit_code) != 0) {
        printf("[VERBOSE] Polecenie zakończone z kodem: %d\n", WEXITSTATUS(exit_code));
    }
    
    return WEXITSTATUS(exit_code);
}

/**
 * @brief Wykonuje polecenie systemowe bez odczytu wyjścia
 */
int execute_command_no_output(const char *command, int verbose) {
    if (verbose) {
        printf("[VERBOSE] Wykonywanie: %s\n", command);
    }
    
    int status = system(command);
    if (status == -1) {
        fprintf(stderr, "Błąd podczas wykonywania polecenia: %s\n", command);
        return -1;
    }
    
    int exit_code = WEXITSTATUS(status);
    if (exit_code != 0) {
        fprintf(stderr, "Polecenie zakończone z błędem (kod %d): %s\n", exit_code, command);
        return -1;
    }
    
    return 0;
}

/**
 * @brief Pobiera informacje o sterowniku dla interfejsu
 */
void get_driver_info(struct wireless_interface *interface, int verbose) {
    char command[MAX_CMD_LEN];
    char buffer[MAX_BUF_LEN];
    
    snprintf(command, sizeof(command), "ethtool -i %s 2>/dev/null", interface->name);
    
    if (execute_command_get_output(command, buffer, sizeof(buffer), verbose) == 0) {
        char *driver_line = strstr(buffer, "driver:");
        if (driver_line) {
            sscanf(driver_line + 7, "%63s", interface->driver);
        } else {
            strcpy(interface->driver, "Nieznany");
        }
    } else {
        strcpy(interface->driver, "Nieznany");
    }
}

/**
 * @brief Pobiera zakres obsługiwanych mocy dla interfejsu
 */
void get_power_range(struct wireless_interface *interface, int verbose) {
    char command[MAX_CMD_LEN];
    char buffer[MAX_BUF_LEN];
    
    snprintf(command, sizeof(command), "iw phy | grep -A 20 'Wiphy.*%s' | grep 'dBm'", interface->name);
    
    interface->min_power = MIN_POWER_DBM;
    interface->max_power = MAX_POWER_DBM;
    
    if (execute_command_get_output(command, buffer, sizeof(buffer), verbose) == 0) {
        // Parsowanie zakresu mocy - uproszczona implementacja
        char *line = strtok(buffer, "\n");
        while (line != NULL) {
            if (strstr(line, "dBm")) {
                // Można rozszerzyć parsowanie dla bardziej precyzyjnego zakresu
                break;
            }
            line = strtok(NULL, "\n");
        }
    }
}

/**
 * @brief Parsuje wyjście polecenia 'iw dev' w poszukiwaniu interfejsów
 */
int parse_interfaces(char *output, struct wireless_interface *interfaces, int verbose) {
    int count = 0;
    char *line = strtok(output, "\n");
    
    while (line != NULL && count < MAX_INTERFACES) {
        // Szukamy linii z "Interface"
        char *iface_ptr = strstr(line, "Interface ");
        if (iface_ptr) {
            char temp_name[MAX_IFACE_LEN];
            if (sscanf(iface_ptr + strlen("Interface "), "%31s", temp_name) == 1) {
                strcpy(interfaces[count].name, temp_name);
                
                // Pobierz dodatkowe informacje
                get_driver_info(&interfaces[count], verbose);
                get_power_range(&interfaces[count], verbose);
                
                count++;
            }
        }
        line = strtok(NULL, "\n");
    }
    
    return count;
}

/**
 * @brief Pobiera aktualną moc nadawania dla interfejsu
 */
void get_current_tx_power(struct wireless_interface *interface, int verbose) {
    char command[MAX_CMD_LEN];
    char buffer[MAX_BUF_LEN];
    
    snprintf(command, sizeof(command), "iw dev %s link 2>/dev/null || iw dev %s info", 
             interface->name, interface->name);

    strcpy(interface->tx_power, "Nieznana");
    interface->power_dbm = -1;

    if (execute_command_get_output(command, buffer, sizeof(buffer), verbose) == 0) {
        // Szukamy wzorca "txpower" lub "tx power"
        char *tx_ptr = strstr(buffer, "txpower");
        if (!tx_ptr) {
            tx_ptr = strstr(buffer, "tx power");
        }
        
        if (tx_ptr) {
            // Przesuwamy wskaźnik za słowo kluczowe
            while (*tx_ptr && !isspace(*tx_ptr)) tx_ptr++;
            while (*tx_ptr && isspace(*tx_ptr)) tx_ptr++;
            
            // Kopiujemy wartość mocy
            char temp_power[MAX_POWER_STR_LEN];
            if (sscanf(tx_ptr, "%63[^\n\r]", temp_power) == 1) {
                strcpy(interface->tx_power, temp_power);
                
                // Wyciągnij wartość numeryczną
                float power_float;
                if (sscanf(temp_power, "%f", &power_float) == 1) {
                    interface->power_dbm = (int)power_float;
                }
            }
        } else {
            // Próba alternatywnego polecenia
            snprintf(command, sizeof(command), "iwconfig %s 2>/dev/null", interface->name);
            if (execute_command_get_output(command, buffer, sizeof(buffer), verbose) == 0) {
                char *tx_ptr = strstr(buffer, "Tx-Power");
                if (tx_ptr) {
                    sscanf(tx_ptr, "Tx-Power=%63[^\n\r ]", interface->tx_power);
                    float power_float;
                    if (sscanf(interface->tx_power, "%f", &power_float) == 1) {
                        interface->power_dbm = (int)power_float;
                    }
                }
            }
        }
    }
}

/**
 * @brief Waliduje czy podana moc jest w obsługiwanym zakresie
 */
int validate_power(const struct wireless_interface *interface, int power_dbm) {
    if (power_dbm < interface->min_power || power_dbm > interface->max_power) {
        fprintf(stderr, "Błąd: Moc %d dBm jest poza obsługiwanym zakresem dla %s (%d-%d dBm)\n",
                power_dbm, interface->name, interface->min_power, interface->max_power);
        return 0;
    }
    return 1;
}

/**
 * @brief Ustawia nową moc nadawania dla interfejsu
 */
int set_tx_power(const char *interface_name, int power_dbm, int verbose) {
    char command[MAX_CMD_LEN];

    printf("\n=== Konfigurowanie interfejsu '%s' ===\n", interface_name);

    // 1. Sprawdź czy interfejs istnieje
    printf("1. Sprawdzanie interfejsu '%s'...\n", interface_name);
    snprintf(command, sizeof(command), "ip link show %s > /dev/null 2>&1", interface_name);
    if (execute_command_no_output(command, verbose) != 0) {
        fprintf(stderr, "Błąd: Interfejs '%s' nie istnieje\n", interface_name);
        return -1;
    }

    // 2. Wyłączenie interfejsu
    printf("2. Wyłączanie interfejsu '%s'...\n", interface_name);
    snprintf(command, sizeof(command), "ip link set %s down 2>/dev/null", interface_name);
    if (execute_command_no_output(command, verbose) != 0) {
        fprintf(stderr, "Ostrzeżenie: Nie udało się wyłączyć interfejsu\n");
    }

    // 3. Ustawienie nowej mocy
    printf("3. Ustawianie mocy nadawania na %d dBm...\n", power_dbm);
    snprintf(command, sizeof(command), "iw dev %s set txpower fixed %d00", interface_name, power_dbm);
    if (execute_command_no_output(command, verbose) != 0) {
        fprintf(stderr, "Błąd: Nie udało się ustawić mocy. Próba przywrócenia interfejsu...\n");
        
        // Próba włączenia interfejsu z powrotem
        snprintf(command, sizeof(command), "ip link set %s up 2>/dev/null", interface_name);
        execute_command_no_output(command, verbose);
        return -1;
    }

    // 4. Włączenie interfejsu
    printf("4. Włączanie interfejsu '%s'...\n", interface_name);
    snprintf(command, sizeof(command), "ip link set %s up", interface_name);
    if (execute_command_no_output(command, verbose) != 0) {
        fprintf(stderr, "Ostrzeżenie: Interfejs skonfigurowany, ale może wymagać ręcznego włączenia\n");
        return 0; // Nie traktujemy jako krytyczny błąd
    }

    printf("\n✓ Konfiguracja zakończona pomyślnie!\n");
    return 0;
}

/**
 * @brief Parsuje argumenty wiersza poleceń
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

    // Inicjalizacja domyślnych wartości
    memset(options, 0, sizeof(struct program_options));

    while ((c = getopt_long(argc, argv, "i:p:lvh", long_options, NULL)) != -1) {
        switch (c) {
            case 'i':
                strncpy(options->interface, optarg, MAX_IFACE_LEN - 1);
                options->interface[MAX_IFACE_LEN - 1] = '\0';
                break;
            case 'p':
                options->power = atoi(optarg);
                if (options->power < MIN_POWER_DBM || options->power > MAX_POWER_DBM) {
                    fprintf(stderr, "Błąd: Moc musi być w zakresie %d-%d dBm\n", 
                            MIN_POWER_DBM, MAX_POWER_DBM);
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

    // Sprawdź czy tryb wsadowy jest kompletny
    if (strlen(options->interface) > 0 && options->power > 0) {
        options->batch_mode = 1;
    } else if (strlen(options->interface) > 0 || options->power > 0) {
        fprintf(stderr, "Błąd: W trybie wsadowym musisz podać zarówno interfejs (-i) jak i moc (-p)\n");
        return -1;
    }

    return 0;
}

/**
 * @brief Tryb interaktywny
 */
int interactive_mode(struct wireless_interface *interfaces, int interface_count, int verbose) {
    printf("\nDostępne interfejsy bezprzewodowe:\n");
    printf("%-4s %-12s %-20s %-15s %-10s\n", "Nr", "Interfejs", "Aktualna moc", "Sterownik", "Zakres");
    printf("%-4s %-12s %-20s %-15s %-10s\n", "---", "----------", "---------------", "----------", "-------");
    
    for (int i = 0; i < interface_count; i++) {
        get_current_tx_power(&interfaces[i], verbose );
        printf("%-4d %-12s %-20s %-15s %d-%d dBm\n", 
               i + 1, interfaces[i].name, interfaces[i].tx_power, 
               interfaces[i].driver, interfaces[i].min_power, interfaces[i].max_power);
    }

    int choice = 0;
    printf("\nWybierz interfejs (podaj numer 1-%d): ", interface_count);
    if (scanf("%d", &choice) != 1 || choice < 1 || choice > interface_count) {
        fprintf(stderr, "Nieprawidłowy wybór.\n");
        return 1;
    }

    struct wireless_interface *selected = &interfaces[choice - 1];

    int power = 0;
    printf("Podaj nową moc nadawania dla '%s' (%d-%d dBm): ", 
           selected->name, selected->min_power, selected->max_power);
    if (scanf("%d", &power) != 1) {
        fprintf(stderr, "Nieprawidłowa wartość. Proszę podać liczbę.\n");
        return 1;
    }

    if (!validate_power(selected, power)) {
        return 1;
    }

    if (set_tx_power(selected->name, power, verbose) == 0) {
        printf("\n=== Sprawdzanie nowej konfiguracji ===\n");
        sleep(1); // Krótka pauza na zastosowanie zmian
        get_current_tx_power(selected, verbose);
        printf("Nowa moc dla interfejsu '%s': %s\n", selected->name, selected->tx_power);
    } else {
        fprintf(stderr, "\nWystąpił błąd podczas konfiguracji interfejsu.\n");
        return 1;
    }

    return 0;
}

int main(int argc, char *argv[]) {
    struct program_options options;
    
    // Parsowanie argumentów
    if (parse_arguments(argc, argv, &options) != 0) {
        return 1;
    }

    // Sprawdzenie uprawnień
    if (geteuid() != 0) {
        fprintf(stderr, "Ten program wymaga uprawnień administratora (root).\n");
        fprintf(stderr, "Uruchom go używając 'sudo %s'\n", argv[0]);
        return 1;
    }

    printf("=== Narzędzie do sterowania mocą karty bezprzewodowej v2.0 ===\n");

    // Pobranie listy interfejsów
    char buffer[MAX_BUF_LEN];
    if (execute_command_get_output("iw dev", buffer, sizeof(buffer), options.verbose) != 0) {
        fprintf(stderr, "Błąd: Nie można pobrać listy interfejsów. Czy narzędzie 'iw' jest zainstalowane?\n");
        return 1;
    }

    struct wireless_interface interfaces[MAX_INTERFACES];
    int interface_count = parse_interfaces(buffer, interfaces, options.verbose);

    if (interface_count == 0) {
        printf("\nNie znaleziono żadnych interfejsów bezprzewodowych.\n");
        printf("Upewnij się, że:\n");
        printf("- Karta Wi-Fi jest włączona\n");
        printf("- Sterowniki są poprawnie zainstalowane\n");
        printf("- Narzędzie 'iw' jest dostępne\n");
        return 1;
    }

    // Tryb tylko listowania
    if (options.list_only) {
        printf("\nZnalezione interfejsy bezprzewodowe (%d):\n", interface_count);
        printf("%-12s %-20s %-15s %-10s\n", "Interfejs", "Aktualna moc", "Sterownik", "Zakres");
        printf("%-12s %-20s %-15s %-10s\n", "----------", "---------------", "----------", "-------");
        
        for (int i = 0; i < interface_count; i++) {
            get_current_tx_power(&interfaces[i], options.verbose);
            printf("%-12s %-20s %-15s %d-%d dBm\n", 
                   interfaces[i].name, interfaces[i].tx_power, 
                   interfaces[i].driver, interfaces[i].min_power, interfaces[i].max_power);
        }
        return 0;
    }

    // Tryb wsadowy
    if (options.batch_mode) {
        // Znajdź wybrany interfejs
        struct wireless_interface *selected = NULL;
        for (int i = 0; i < interface_count; i++) {
            if (strcmp(interfaces[i].name, options.interface) == 0) {
                selected = &interfaces[i];
                break;
            }
        }

        if (!selected) {
            fprintf(stderr, "Błąd: Interfejs '%s' nie został znaleziony.\n", options.interface);
            printf("Dostępne interfejsy: ");
            for (int i = 0; i < interface_count; i++) {
                printf("%s%s", interfaces[i].name, (i < interface_count - 1) ? ", " : "\n");
            }
            return 1;
        }

        if (!validate_power(selected, options.power)) {
            return 1;
        }

        printf("Tryb wsadowy: Ustawianie mocy %d dBm dla interfejsu '%s'\n", 
               options.power, options.interface);

        if (set_tx_power(selected->name, options.power, options.verbose) == 0) {
            printf("\n=== Sprawdzanie nowej konfiguracji ===\n");
            sleep(1);
            get_current_tx_power(selected, options.verbose);
            printf("Nowa moc dla interfejsu '%s': %s\n", selected->name, selected->tx_power);
            return 0;
        } else {
            return 1;
        }
    }

    // Tryb interaktywny
    return interactive_mode(interfaces, interface_count, options.verbose);
}