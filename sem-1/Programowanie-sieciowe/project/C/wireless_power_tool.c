/**
 * gcc -Wall wireless_power_tool.c -o wireless_power_tool
 * sudo ./wireless_power_tool
 * 
 * $ sudo ./wireless_power_tool
--- Narzędzie do sterowania mocą karty bezprzewodowej (C) ---

Dostępne interfejsy bezprzewodowe:
1. wlan0 (Aktualna moc: 20.00 dBm)

Wybierz interfejs (podaj numer): 1
Podaj nową moc nadawania dla 'wlan0' (w dBm): 15

Konfigurowanie interfejsu 'wlan0'...
1. Wyłączanie interfejsu 'wlan0'...
2. Ustawianie mocy nadawania na 15 dBm...
3. Włączanie interfejsu 'wlan0'...

Konfiguracja zakończona pomyślnie!

Sprawdzanie nowej konfiguracji...
Nowa moc dla interfejsu 'wlan0': 15.00 dBm
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h> // Dla geteuid()
#include <ctype.h>  // Dla isspace()

#define MAX_CMD_LEN 256      // Maksymalna długość polecenia
#define MAX_BUF_LEN 1024     // Maksymalna wielkość bufora dla odpowiedzi
#define MAX_INTERFACES 10    // Maksymalna liczba obsługiwanych interfejsów
#define MAX_IFACE_LEN 16     // Maksymalna długość nazwy interfejsu

// Struktura do przechowywania informacji o interfejsie
struct wireless_interface {
    char name[MAX_IFACE_LEN];
    char tx_power[32];
};

/**
 * @brief Wykonuje polecenie systemowe i zapisuje jego wyjście do bufora.
 *
 * @param command Polecenie do wykonania.
 * @param buffer Bufor, do którego zostanie zapisane wyjście.
 * @param buffer_size Rozmiar bufora.
 * @return int 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int execute_command_get_output(const char *command, char *buffer, size_t buffer_size) {
    FILE *fp;
    fp = popen(command, "r");
    if (fp == NULL) {
        perror("Błąd podczas uruchamiania polecenia (popen)");
        return -1;
    }

    size_t bytes_read = fread(buffer, 1, buffer_size - 1, fp);
    buffer[bytes_read] = '\0'; // Zapewnienie zakończenia stringa

    if (pclose(fp) == -1) {
        perror("Błąd podczas zamykania strumienia (pclose)");
        // Kontynuujemy, bo mogliśmy odczytać dane, nawet jeśli pclose zwróci błąd
    }
    return 0;
}

/**
 * @brief Wykonuje polecenie systemowe, które nie wymaga odczytu wyjścia.
 *
 * @param command Polecenie do wykonania.
 * @return int 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int execute_command_no_output(const char *command) {
    int status = system(command);
    if (status == -1 || WEXITSTATUS(status) != 0) {
        fprintf(stderr, "Błąd podczas wykonywania polecenia: %s\n", command);
        return -1;
    }
    return 0;
}

/**
 * @brief Parsuje wyjście polecenia 'iw dev' w poszukiwaniu nazw interfejsów.
 *
 * @param output Tekst do sparsowania.
 * @param interfaces Tablica struktur do wypełnienia.
 * @return int Liczba znalezionych interfejsów.
 */
int parse_interfaces(char *output, struct wireless_interface *interfaces) {
    int count = 0;
    char *line = strtok(output, "\n");
    while (line != NULL && count < MAX_INTERFACES) {
        char *iface_ptr = strstr(line, "Interface ");
        if (iface_ptr) {
            sscanf(iface_ptr + strlen("Interface "), "%15s", interfaces[count].name);
            count++;
        }
        line = strtok(NULL, "\n");
    }
    return count;
}

/**
 * @brief Pobiera i parsuje aktualną moc nadawania dla danego interfejsu.
 *
 * @param interface Struktura interfejsu do zaktualizowania.
 */
void get_current_tx_power(struct wireless_interface *interface) {
    char command[MAX_CMD_LEN];
    char buffer[MAX_BUF_LEN];
    snprintf(command, sizeof(command), "iw dev %s link", interface->name);

    strcpy(interface->tx_power, "Nieznana"); // Wartość domyślna

    if (execute_command_get_output(command, buffer, sizeof(buffer)) == 0) {
        char *tx_ptr = strstr(buffer, "txpower");
        if (tx_ptr) {
            // Przesuwamy wskaźnik za słowo "txpower" i białe znaki
            tx_ptr += strlen("txpower");
            while (*tx_ptr && isspace(*tx_ptr)) {
                tx_ptr++;
            }
            // Kopiujemy wartość mocy (np. "20.00 dBm")
            sscanf(tx_ptr, "%31[^\n]", interface->tx_power);
        }
    }
}


/**
 * @brief Ustawia nową moc nadawania dla wybranego interfejsu.
 *
 * @param interface_name Nazwa interfejsu.
 * @param power_dbm Moc w dBm.
 * @return int 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int set_tx_power(const char *interface_name, int power_dbm) {
    char command[MAX_CMD_LEN];

    printf("\nKonfigurowanie interfejsu '%s'...\n", interface_name);

    // 1. Wyłączenie interfejsu
    printf("1. Wyłączanie interfejsu '%s'...\n", interface_name);
    snprintf(command, sizeof(command), "ip link set %s down", interface_name);
    if (execute_command_no_output(command) != 0) return -1;

    // 2. Ustawienie nowej mocy
    printf("2. Ustawianie mocy nadawania na %d dBm...\n", power_dbm);
    snprintf(command, sizeof(command), "iw dev %s set txpower fixed %dmBm", interface_name, power_dbm);
    if (execute_command_no_output(command) != 0) {
        fprintf(stderr, "Nie udało się ustawić mocy. Próba przywrócenia interfejsu do stanu początkowego.\n");
        snprintf(command, sizeof(command), "ip link set %s up", interface_name);
        execute_command_no_output(command); // Spróbuj włączyć interfejs z powrotem
        return -1;
    }

    // 3. Włączenie interfejsu
    printf("3. Włączanie interfejsu '%s'...\n", interface_name);
    snprintf(command, sizeof(command), "ip link set %s up", interface_name);
    if (execute_command_no_output(command) != 0) return -1;

    printf("\nKonfiguracja zakończona pomyślnie!\n");
    return 0;
}


int main() {
    if (geteuid() != 0) {
        fprintf(stderr, "Ten program wymaga uprawnień administratora (root).\n");
        fprintf(stderr, "Uruchom go używając 'sudo ./twoj_program'\n");
        return 1;
    }

    printf("--- Narzędzie do sterowania mocą karty bezprzewodowej (C) ---\n");

    char buffer[MAX_BUF_LEN];
    if (execute_command_get_output("iw dev", buffer, sizeof(buffer)) != 0) {
        return 1;
    }

    struct wireless_interface interfaces[MAX_INTERFACES];
    int interface_count = parse_interfaces(buffer, interfaces);

    if (interface_count == 0) {
        printf("\nNie znaleziono żadnych interfejsów bezprzewodowych.\n");
        printf("Upewnij się, że karta Wi-Fi jest włączona i sterowniki są poprawnie zainstalowane.\n");
        return 1;
    }

    printf("\nDostępne interfejsy bezprzewodowe:\n");
    for (int i = 0; i < interface_count; i++) {
        get_current_tx_power(&interfaces[i]);
        printf("%d. %s (Aktualna moc: %s)\n", i + 1, interfaces[i].name, interfaces[i].tx_power);
    }

    int choice = 0;
    printf("\nWybierz interfejs (podaj numer): ");
    if (scanf("%d", &choice) != 1 || choice < 1 || choice > interface_count) {
        fprintf(stderr, "Nieprawidłowy wybór.\n");
        return 1;
    }
    const char *selected_interface_name = interfaces[choice - 1].name;

    int power = 0;
    printf("Podaj nową moc nadawania dla '%s' (w dBm): ", selected_interface_name);
    if (scanf("%d", &power) != 1) {
        fprintf(stderr, "Nieprawidłowa wartość. Proszę podać liczbę.\n");
        return 1;
    }

    if (set_tx_power(selected_interface_name, power) == 0) {
        printf("\nSprawdzanie nowej konfiguracji...\n");
        // Aktualizacja informacji o mocy po zmianie
        get_current_tx_power(&interfaces[choice - 1]);
        printf("Nowa moc dla interfejsu '%s': %s\n", interfaces[choice-1].name, interfaces[choice-1].tx_power);
    } else {
        fprintf(stderr, "\nWystąpił błąd podczas konfiguracji interfejsu.\n");
        return 1;
    }

    return 0;
}