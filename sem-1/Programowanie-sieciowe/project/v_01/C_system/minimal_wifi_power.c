/**
 * Minimalny automatyczny menedżer mocy karty bezprzewodowej
 * Kompilacja: gcc -Wall -O2 minimal_wifi_power.c -o minimal_wifi_power -lm
 * Uruchomienie: sudo ./minimal_wifi_power wlan0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <linux/wireless.h>
#include <linux/if.h>
#include <net/if.h>
#include <math.h>

#define MIN_SIGNAL_THRESHOLD -75  // Próg słabego sygnału
#define MIN_TX_POWER 5            // Minimalna moc
#define MAX_TX_POWER 20           // Maksymalna moc
#define POWER_STEP 2              // Krok zmiany mocy
#define CHECK_INTERVAL 5          // Interwał sprawdzania w sekundach

/**
 * @brief Pobiera siłę sygnału Wi-Fi używając ioctl
 */
int get_signal_strength(const char *interface) {
    int sock_fd;
    struct iwreq wrq;
    struct iw_statistics iw_stats;
    
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        return -999;
    }
    
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    
    wrq.u.data.pointer = &iw_stats;
    wrq.u.data.length = sizeof(iw_stats);
    wrq.u.data.flags = 1;
    
    if (ioctl(sock_fd, SIOCGIWSTATS, &wrq) < 0) {
        close(sock_fd);
        return -999;
    }
    
    close(sock_fd);
    
    // Konwersja na dBm
    if (iw_stats.qual.updated & IW_QUAL_DBM) {
        return iw_stats.qual.level - 256;
    } else {
        return iw_stats.qual.level - 100;
    }
}

/**
 * @brief Pobiera aktualną moc nadawania używając ioctl
 */
int get_tx_power(const char *interface) {
    int sock_fd;
    struct iwreq wrq;
    
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        return -1;
    }
    
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    
    if (ioctl(sock_fd, SIOCGIWTXPOW, &wrq) < 0) {
        close(sock_fd);
        return -1;
    }
    
    close(sock_fd);
    
    if (wrq.u.txpower.flags & IW_TXPOW_DBM) {
        return wrq.u.txpower.value;
    } else if (wrq.u.txpower.flags & IW_TXPOW_MWATT) {
        if (wrq.u.txpower.value > 0) {
            return (int)(10.0 * log10(wrq.u.txpower.value));
        }
    }
    
    return -1;
}

/**
 * @brief Ustawia moc nadawania używając ioctl
 */
int set_tx_power(const char *interface, int power_dbm) {
    int sock_fd;
    struct iwreq wrq;
    struct ifreq ifr;
    
    // Ogranicz moc do dozwolonego zakresu
    if (power_dbm < MIN_TX_POWER) power_dbm = MIN_TX_POWER;
    if (power_dbm > MAX_TX_POWER) power_dbm = MAX_TX_POWER;
    
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        return -1;
    }
    
    // Wyłącz interfejs
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, interface, IFNAMSIZ - 1);
    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) {
        if (ifr.ifr_flags & IFF_UP) {
            ifr.ifr_flags &= ~IFF_UP;
            ioctl(sock_fd, SIOCSIFFLAGS, &ifr);
            usleep(100000); // 100ms
        }
    }
    
    // Ustaw moc
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    wrq.u.txpower.value = power_dbm;
    wrq.u.txpower.flags = IW_TXPOW_DBM | IW_TXPOW_FIXED;
    wrq.u.txpower.disabled = 0;
    
    if (ioctl(sock_fd, SIOCSIWTXPOW, &wrq) < 0) {
        close(sock_fd);
        return -1;
    }
    
    // Włącz interfejs z powrotem
    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) {
        ifr.ifr_flags |= IFF_UP;
        ioctl(sock_fd, SIOCSIFFLAGS, &ifr);
    }
    
    close(sock_fd);
    printf("Ustawiono moc na %d dBm\n", power_dbm);
    return 0;
}

/**
 * @brief Główna funkcja programu
 */
int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Użycie: %s <interfejs>\n", argv[0]);
        printf("Przykład: %s wlan0\n", argv[0]);
        return 1;
    }
    
    if (geteuid() != 0) {
        printf("Błąd: Program wymaga uprawnień root\n");
        return 1;
    }
    
    const char *interface = argv[1];
    
    printf("Minimalny menedżer mocy Wi-Fi dla %s\n", interface);
    printf("Próg sygnału: %d dBm, Moc: %d-%d dBm\n", 
           MIN_SIGNAL_THRESHOLD, MIN_TX_POWER, MAX_TX_POWER);
    
    while (1) {
        int signal = get_signal_strength(interface);
        int current_power = get_tx_power(interface);
        
        if (signal == -999 || current_power == -1) {
            printf("Błąd: Nie można odczytać parametrów interfejsu %s\n", interface);
            sleep(CHECK_INTERVAL);
            continue;
        }
        
        printf("Sygnał: %d dBm, Moc: %d dBm", signal, current_power);
        
        // Prosta logika dostosowania mocy
        if (signal < MIN_SIGNAL_THRESHOLD) {
            // Słaby sygnał - zwiększ moc
            int new_power = current_power + POWER_STEP;
            if (new_power <= MAX_TX_POWER) {
                printf(" -> Zwiększam moc do %d dBm\n", new_power);
                set_tx_power(interface, new_power);
            } else {
                printf(" -> Moc już na maksimum\n");
            }
        } else if (signal > MIN_SIGNAL_THRESHOLD + 10) {
            // Dobry sygnał - zmniejsz moc dla oszczędności
            int new_power = current_power - POWER_STEP;
            if (new_power >= MIN_TX_POWER) {
                printf(" -> Zmniejszam moc do %d dBm\n", new_power);
                set_tx_power(interface, new_power);
            } else {
                printf(" -> Moc już na minimum\n");
            }
        } else {
            printf(" -> Moc optymalna\n");
        }
        
        sleep(CHECK_INTERVAL);
    }
    
    return 0;
}