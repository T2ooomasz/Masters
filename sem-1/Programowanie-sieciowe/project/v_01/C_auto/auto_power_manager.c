/**
 * gcc -Wall auto_power_manager.c -o auto_power_manager
 * Przykładowe uruchomienie:
Bash

sudo ./auto_power_manager wlan0 -75 -65 5 20 10
Powyższe polecenie uruchomi menedżer dla interfejsu wlan0. Co 10 sekund będzie on sprawdzał sygnał:

Jeśli sygnał spadnie poniżej -75 dBm, moc zostanie zwiększona o 1 dBm (ale nie powyżej 20 dBm).
Jeśli sygnał wzrośnie powyżej -65 dBm, moc zostanie zmniejszona o 1 dBm (ale nie poniżej 5 dBm).
Uruchomienie w tle:
Aby program działał trwale po zamknięciu terminala, można go uruchomić za pomocą nohup i przekierować jego wyjście do pliku logu:

Bash

sudo nohup ./auto_power_manager wlan0 -75 -65 5 20 10 > wifi_power.log 2>&1 &
Aby zatrzymać program, należy znaleźć jego PID (pgrep auto_power_manager) i użyć komendy sudo kill <PID>.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <limits.h>

#define MAX_CMD_LEN 256
#define MAX_BUF_LEN 1024

// --- Funkcje pomocnicze ---

/**
 * @brief Wyświetla log z aktualnym czasem.
 */
void log_message(const char *message) {
    time_t now = time(NULL);
    char time_buf[20];
    strftime(time_buf, sizeof(time_buf), "%Y-%m-%d %H:%M:%S", localtime(&now));
    printf("[%s] %s\n", time_buf, message);
    fflush(stdout); // Wymuszenie opróżnienia bufora, ważne dla logów
}

/**
 * @brief Wykonuje polecenie i zwraca jego wyjście w buforze.
 * @return 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int execute_command_get_output(const char *command, char *buffer, size_t buffer_size) {
    FILE *fp = popen(command, "r");
    if (fp == NULL) {
        perror("popen failed");
        return -1;
    }
    if (fgets(buffer, buffer_size, fp) == NULL) {
        pclose(fp);
        return -1; // Brak wyjścia lub błąd odczytu
    }
    pclose(fp);
    return 0;
}

/**
 * @brief Pobiera aktualną siłę sygnału dla danego interfejsu.
 * @return Wartość siły sygnału w dBm lub INT_MIN w przypadku błędu.
 */
int get_signal_strength(const char *interface) {
    char cmd[MAX_CMD_LEN];
    char buf[MAX_BUF_LEN];
    snprintf(cmd, sizeof(cmd), "iw dev %s link", interface);

    if (execute_command_get_output(cmd, buf, sizeof(buf)) != 0 || strstr(buf, "Not connected")) {
        return INT_MIN; // Błąd lub interfejs nie jest połączony
    }

    char *signal_ptr = strstr(buf, "signal:");
    if (signal_ptr) {
        int signal_dbm = 0;
        // Parsuje wartość numeryczną po "signal: "
        if (sscanf(signal_ptr, "signal: %d dBm", &signal_dbm) == 1) {
            return signal_dbm;
        }
    }
    return INT_MIN;
}

/**
 * @brief Pobiera aktualną moc nadawania dla interfejsu.
 * @return Wartość mocy w dBm lub INT_MIN w przypadku błędu.
 */
int get_current_tx_power(const char *interface) {
    char cmd[MAX_CMD_LEN];
    char buf[MAX_BUF_LEN];
    snprintf(cmd, sizeof(cmd), "iw dev %s info", interface); // 'info' jest bardziej niezawodne dla txpower

    if (execute_command_get_output(cmd, buf, sizeof(buf)) != 0) {
        return INT_MIN;
    }

    char *txpower_ptr = strstr(buf, "txpower");
    if (txpower_ptr) {
        int power_dbm = 0;
        // Parsuje wartość numeryczną, np. "20.00 dBm"
        if (sscanf(txpower_ptr, "txpower %d.00 dBm", &power_dbm) == 1) {
            return power_dbm;
        }
    }
    return INT_MIN;
}

/**
 * @brief Ustawia nową moc nadawania (bez restartu interfejsu).
 * @return 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int set_tx_power(const char *interface, int power_dbm) {
    char cmd[MAX_CMD_LEN];
    char log_buf[MAX_CMD_LEN + 50];
    snprintf(cmd, sizeof(cmd), "sudo iw dev %s set txpower fixed %dmBm", interface, power_dbm);
    
    snprintf(log_buf, sizeof(log_buf), "Wykonuję: %s", cmd);
    log_message(log_buf);

    if (system(cmd) != 0) {
        log_message("Błąd podczas ustawiania nowej mocy.");
        return -1;
    }
    return 0;
}


void print_usage(const char *prog_name) {
    fprintf(stderr, "Użycie: %s <interfejs> <min_sygnal> <opt_sygnal> <min_moc> <max_moc> <interwal_s>\n", prog_name);
    fprintf(stderr, "  <interfejs>    - Nazwa interfejsu Wi-Fi (np. wlan0)\n");
    fprintf(stderr, "  <min_sygnal>   - Dolny próg sygnału w dBm (np. -75). Poniżej tej wartości moc wzrośnie.\n");
    fprintf(stderr, "  <opt_sygnal>   - Górny próg sygnału w dBm (np. -65). Powyżej tej wartości moc spadnie.\n");
    fprintf(stderr, "  <min_moc>      - Minimalna dozwolona moc nadawania w dBm (np. 5).\n");
    fprintf(stderr, "  <max_moc>      - Maksymalna dozwolona moc nadawania w dBm (np. 20).\n");
    fprintf(stderr, "  <interwal_s>   - Czas w sekundach między kolejnymi sprawdzeniami (np. 10).\n\n");
    fprintf(stderr, "Przykład: sudo %s wlan0 -75 -65 5 20 10\n", prog_name);
}

// --- Główna logika ---
int main(int argc, char *argv[]) {
    if (argc != 7) {
        print_usage(argv[0]);
        return 1;
    }

    // --- Parsowanie argumentów ---
    const char *interface = argv[1];
    int min_signal_threshold = atoi(argv[2]);
    int optimal_signal_threshold = atoi(argv[3]);
    int min_tx_power = atoi(argv[4]);
    int max_tx_power = atoi(argv[5]);
    int interval_sec = atoi(argv[6]);
    const int power_step = 1; // Krok zmiany mocy (w dBm)

    // --- Sprawdzenie uprawnień i warunków początkowych ---
    if (geteuid() != 0) {
        fprintf(stderr, "Błąd: Ten program wymaga uprawnień administratora (root).\n");
        return 1;
    }
    if (min_signal_threshold >= optimal_signal_threshold || min_tx_power >= max_tx_power) {
        fprintf(stderr, "Błąd: Nieprawidłowe wartości progów lub mocy.\n");
        return 1;
    }
     if (get_signal_strength(interface) == INT_MIN) {
        fprintf(stderr, "Błąd: Interfejs '%s' nie jest połączony z siecią lub nie istnieje.\n", interface);
        return 1;
    }

    char log_buf[256];
    log_message("Uruchomiono automatyczny menedżer mocy Wi-Fi.");
    snprintf(log_buf, sizeof(log_buf), "Konfiguracja: Interfejs=%s, ProgiSygnału=[%d, %d] dBm, ZakresMocy=[%d, %d] dBm, Interwał=%ds",
             interface, min_signal_threshold, optimal_signal_threshold, min_tx_power, max_tx_power, interval_sec);
    log_message(log_buf);


    // --- Główna pętla monitorująca ---
    while (1) {
        int current_signal = get_signal_strength(interface);
        int current_power = get_current_tx_power(interface);

        if (current_signal == INT_MIN) {
            log_message("Utracono połączenie. Oczekiwanie na ponowne połączenie...");
            sleep(interval_sec);
            continue;
        }

        snprintf(log_buf, sizeof(log_buf), "Stan: Sygnał=%d dBm, Moc=%d dBm.", current_signal, (current_power == INT_MIN ? -1 : current_power));
        log_message(log_buf);

        if (current_power == INT_MIN) {
            log_message("Nie można odczytać mocy. Pomijam cykl.");
        } 
        // Decyzja o zmianie mocy
        else if (current_signal < min_signal_threshold) {
            log_message("Sygnał za słaby. Zwiększam moc...");
            int new_power = current_power + power_step;
            if (new_power > max_tx_power) new_power = max_tx_power;

            if (new_power > current_power) {
                set_tx_power(interface, new_power);
            } else {
                log_message("Moc jest już na maksymalnym poziomie.");
            }
        } 
        else if (current_signal > optimal_signal_threshold) {
            log_message("Sygnał bardzo dobry. Zmniejszam moc w celu oszczędzania energii...");
            int new_power = current_power - power_step;
            if (new_power < min_tx_power) new_power = min_tx_power;

            if (new_power < current_power) {
                set_tx_power(interface, new_power);
            } else {
                log_message("Moc jest już na minimalnym poziomie.");
            }
        } else {
            log_message("Sygnał stabilny. Brak zmian.");
        }

        sleep(interval_sec);
    }

    return 0; // Ta część nigdy nie zostanie osiągnięta
}