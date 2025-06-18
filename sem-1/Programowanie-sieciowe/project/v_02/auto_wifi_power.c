/**
 * auto_wifi_power.c
 * Prosty automatyczny menedżer mocy karty bezprzewodowej.
 * Program wykorzystuje funkcje systemowe do interakcji z jądrem Linux.
 *
 * Kompilacja:
 *   gcc -Wall -O2 auto_wifi_power.c -o auto_wifi_power -lm
 *
 * Uruchomienie (wymaga uprawnień root):
 *   sudo ./auto_wifi_power <interfejs>
 *   Przykład: sudo ./auto_wifi_power wlan0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>         // Dla geteuid(), close(), nanosleep()
#include <sys/socket.h>     // Dla socket()
#include <sys/ioctl.h>      // Dla ioctl()
#include <linux/wireless.h> // Dla struct iwreq, IW_QUAL_DBM, etc.
#include <net/if.h>         // Dla struct ifreq, IFNAMSIZ, if_nametoindex
#include <linux/if.h>       // Dla flag IFF_UP i innych specyficznych dla Linuksa
#include <math.h>           // Dla log10()
#include <time.h>           // Dla nanosleep(), struct timespec
#include <errno.h>          // Dla perror()

// === Konfiguracja ===
#define LOW_SIGNAL_THRESHOLD   -75  // Próg słabego sygnału (dBm), poniżej którego zwiększamy moc
#define HIGH_SIGNAL_THRESHOLD  -60  // Próg dobrego sygnału (dBm), powyżej którego możemy zmniejszyć moc
#define MIN_TX_POWER           5    // Minimalna moc nadawania (dBm)
#define MAX_TX_POWER           20   // Maksymalna moc nadawania (dBm)
#define POWER_STEP             2    // Krok zmiany mocy (dBm)
#define CHECK_INTERVAL_SEC     5    // Interwał sprawdzania stanu (w sekundach)
#define PAUSE_MS_AFTER_IF_DOWN 100  // Pauza w milisekundach po wyłączeniu interfejsu

// Kod błędu dla funkcji get_signal_strength
#define SIGNAL_READ_ERROR -999

/**
 * @brief Pobiera siłę sygnału Wi-Fi dla danego interfejsu.
 * Wykorzystuje ioctl do komunikacji z jądrem.
 * @param interface Nazwa interfejsu (np. "wlan0").
 * @return Siła sygnału w dBm lub SIGNAL_READ_ERROR w przypadku błędu.
 */
int get_signal_strength(const char *interface) {
    int sock_fd;
    struct iwreq wrq;
    struct iw_statistics iw_stats;

    // Utworzenie gniazda (system call socket())
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        perror("socket (get_signal_strength)");
        return SIGNAL_READ_ERROR;
    }

    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    wrq.ifr_name[IFNAMSIZ - 1] = '\0'; // Zapewnienie null-termination

    wrq.u.data.pointer = &iw_stats;
    wrq.u.data.length = sizeof(iw_stats);
    wrq.u.data.flags = 1; // Chcemy zaktualizowane statystyki

    // Pobranie statystyk bezprzewodowych (system call ioctl())
    if (ioctl(sock_fd, SIOCGIWSTATS, &wrq) < 0) {
        // Nie drukujemy perror tutaj, bo interfejs może być chwilowo niedostępny
        // np. rozłączony. Komunikat błędu pojawi się w pętli głównej.
        close(sock_fd);
        return SIGNAL_READ_ERROR;
    }

    close(sock_fd);

    // Konwersja siły sygnału na dBm
    if (iw_stats.qual.updated & IW_QUAL_DBM) {
        // Wartość jest już w dBm (lub wymaga przeliczenia)
        return iw_stats.qual.level - 256;
    } else {
        // Wartość jest w jednostkach względnych (RSSI), przybliżona konwersja
        return iw_stats.qual.level - 100; // Typowe przybliżenie dla RSSI
    }
}

/**
 * @brief Pobiera aktualną moc nadawania dla danego interfejsu.
 * Wykorzystuje ioctl do komunikacji z jądrem.
 * @param interface Nazwa interfejsu (np. "wlan0").
 * @return Moc nadawania w dBm lub -1 w przypadku błędu.
 */
int get_tx_power(const char *interface) {
    int sock_fd;
    struct iwreq wrq;

    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        perror("socket (get_tx_power)");
        return -1;
    }

    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    wrq.ifr_name[IFNAMSIZ - 1] = '\0';

    // Pobranie mocy nadawania (system call ioctl())
    if (ioctl(sock_fd, SIOCGIWTXPOW, &wrq) < 0) {
        // Podobnie jak w get_signal_strength, błąd może być przejściowy
        close(sock_fd);
        return -1;
    }

    close(sock_fd);

    if (wrq.u.txpower.disabled) {
        return 0; // Moc wyłączona
    }

    // Konwersja mocy na dBm, jeśli jest w mW
    if (wrq.u.txpower.flags & IW_TXPOW_DBM) {
        return wrq.u.txpower.value; // Moc jest już w dBm
    } else if (wrq.u.txpower.flags & IW_TXPOW_MWATT) {
        if (wrq.u.txpower.value > 0) {
            // P(dBm) = 10 * log10(P(mW))
            return (int)(10.0 * log10((double)wrq.u.txpower.value));
        }
    }
    return -1; // Nieznany format lub błąd
}

/**
 * @brief Ustawia moc nadawania dla danego interfejsu.
 * Wykorzystuje ioctl do komunikacji z jądrem.
 * Wymaga tymczasowego wyłączenia i włączenia interfejsu.
 * @param interface Nazwa interfejsu (np. "wlan0").
 * @param power_dbm Docelowa moc nadawania w dBm.
 * @return 0 w przypadku sukcesu, -1 w przypadku błędu.
 */
int set_tx_power(const char *interface, int power_dbm) {
    int sock_fd;
    struct iwreq wrq;
    struct ifreq ifr; // Do zarządzania flagami interfejsu (IFF_UP)

    // Ograniczenie mocy do dozwolonego zakresu
    if (power_dbm < MIN_TX_POWER) power_dbm = MIN_TX_POWER;
    if (power_dbm > MAX_TX_POWER) power_dbm = MAX_TX_POWER;

    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        perror("socket (set_tx_power)");
        return -1;
    }

    // Krok 1: Wyłącz interfejs (jeśli jest włączony)
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, interface, IFNAMSIZ - 1);
    ifr.ifr_name[IFNAMSIZ - 1] = '\0';

    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) { // Pobierz aktualne flagi
        if (ifr.ifr_flags & IFF_UP) { // Jeśli interfejs jest włączony
            ifr.ifr_flags &= ~IFF_UP; // Usuń flagę IFF_UP
            if (ioctl(sock_fd, SIOCSIFFLAGS, &ifr) < 0) { // Ustaw nowe flagi (wyłącz interfejs)
                perror("ioctl SIOCSIFFLAGS (wyłączanie interfejsu)");
                // Kontynuuj próbę ustawienia mocy, ale interfejs mógł pozostać włączony
            } else {
                // Pauza, aby system zdążył przetworzyć zmianę (system call nanosleep())
                struct timespec pause_duration = {0, PAUSE_MS_AFTER_IF_DOWN * 1000000L};
                nanosleep(&pause_duration, NULL);
            }
        }
    } else {
        perror("ioctl SIOCGIFFLAGS (pobieranie flag przed wyłączeniem)");
        // Nie udało się pobrać flag, kontynuuj z ostrożnością
    }

    // Krok 2: Ustaw moc nadawania
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    wrq.ifr_name[IFNAMSIZ - 1] = '\0';

    wrq.u.txpower.value = power_dbm;
    wrq.u.txpower.flags = IW_TXPOW_DBM; // Chcemy ustawić w dBm
    wrq.u.txpower.disabled = 0;         // Włącz zarządzanie mocą (0) vs wyłącz (1)

    if (ioctl(sock_fd, SIOCSIWTXPOW, &wrq) < 0) { // Ustaw moc (system call ioctl())
        perror("ioctl SIOCSIWTXPOW (ustawianie mocy)");
        // Próba przywrócenia interfejsu do stanu UP, nawet jeśli ustawienie mocy się nie powiodło
    }

    // Krok 3: Włącz interfejs z powrotem
    // Ponownie pobierz flagi, aby upewnić się, że modyfikujemy aktualny stan
    memset(&ifr, 0, sizeof(ifr)); // Dla pewności wyczyść strukturę ifr
    strncpy(ifr.ifr_name, interface, IFNAMSIZ - 1);
    ifr.ifr_name[IFNAMSIZ - 1] = '\0';

    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) { // Pobierz aktualne flagi
        ifr.ifr_flags |= IFF_UP; // Dodaj flagę IFF_UP
        if (ioctl(sock_fd, SIOCSIFFLAGS, &ifr) < 0) { // Ustaw nowe flagi (włącz interfejs)
            perror("ioctl SIOCSIFFLAGS (włączanie interfejsu)");
            // Błąd przy włączaniu, ale moc mogła zostać ustawiona
        }
    } else {
        perror("ioctl SIOCGIFFLAGS (pobieranie flag przed włączeniem)");
    }

    close(sock_fd);
    return 0; // Załóżmy sukces, jeśli główne ioctl SIOCSIWTXPOW nie zwróciło błędu krytycznego
}

/**
 * @brief Główna funkcja programu.
 */
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Użycie: %s <interfejs>\n", argv[0]);
        fprintf(stderr, "Przykład: %s wlan0\n", argv[0]);
        return 1;
    }

    // Sprawdzenie uprawnień root (system call geteuid())
    if (geteuid() != 0) {
        fprintf(stderr, "Błąd: Program wymaga uprawnień administratora (root).\n");
        return 1;
    }

    const char *interface = argv[1];

    printf("Automatyczny menedżer mocy Wi-Fi dla interfejsu: %s\n", interface);
    printf("Konfiguracja:\n");
    printf("  Próg słabego sygnału : %d dBm\n", LOW_SIGNAL_THRESHOLD);
    printf("  Próg dobrego sygnału : %d dBm\n", HIGH_SIGNAL_THRESHOLD);
    printf("  Zakres mocy          : %d-%d dBm\n", MIN_TX_POWER, MAX_TX_POWER);
    printf("  Krok zmiany mocy     : %d dBm\n", POWER_STEP);
    printf("  Interwał sprawdzania : %d s\n", CHECK_INTERVAL_SEC);
    printf("--------------------------------------------------\n");


    while (1) {
        int signal_strength = get_signal_strength(interface);
        int current_tx_power = get_tx_power(interface);

        if (signal_strength == SIGNAL_READ_ERROR || current_tx_power == -1) {
            fprintf(stderr, "Błąd: Nie można odczytać parametrów interfejsu %s. Próbuję ponownie...\n", interface);
        } else {
            printf("Stan: Sygnał: %d dBm, Moc: %d dBm", signal_strength, current_tx_power);

            int new_tx_power = current_tx_power;

            if (signal_strength < LOW_SIGNAL_THRESHOLD) {
                // Sygnał jest słaby, spróbuj zwiększyć moc
                if (current_tx_power < MAX_TX_POWER) {
                    new_tx_power = current_tx_power + POWER_STEP;
                    if (new_tx_power > MAX_TX_POWER) {
                        new_tx_power = MAX_TX_POWER;
                    }
                    if (new_tx_power != current_tx_power) {
                         printf(" -> Słaby sygnał. Zwiększam moc do %d dBm.\n", new_tx_power);
                         set_tx_power(interface, new_tx_power);
                    } else {
                        printf(" -> Słaby sygnał, moc już na maksimum (%d dBm).\n", current_tx_power);
                    }
                } else {
                    printf(" -> Słaby sygnał, moc już na maksimum (%d dBm).\n", current_tx_power);
                }
            } else if (signal_strength > HIGH_SIGNAL_THRESHOLD) {
                // Sygnał jest dobry, spróbuj zmniejszyć moc
                if (current_tx_power > MIN_TX_POWER) {
                    new_tx_power = current_tx_power - POWER_STEP;
                    if (new_tx_power < MIN_TX_POWER) {
                        new_tx_power = MIN_TX_POWER;
                    }
                     if (new_tx_power != current_tx_power) {
                        printf(" -> Dobry sygnał. Zmniejszam moc do %d dBm.\n", new_tx_power);
                        set_tx_power(interface, new_tx_power);
                    } else {
                        printf(" -> Dobry sygnał, moc już na minimum (%d dBm).\n", current_tx_power);
                    }
                } else {
                    printf(" -> Dobry sygnał, moc już na minimum (%d dBm).\n", current_tx_power);
                }
            } else {
                // Sygnał w optymalnym zakresie
                printf(" -> Sygnał w normie. Moc optymalna.\n");
            }
        }

        // Oczekiwanie (system call nanosleep())
        struct timespec sleep_duration = {CHECK_INTERVAL_SEC, 0};
        nanosleep(&sleep_duration, NULL);
    }

    return 0; // Teoretycznie nieosiągalne w pętli while(1)
}
