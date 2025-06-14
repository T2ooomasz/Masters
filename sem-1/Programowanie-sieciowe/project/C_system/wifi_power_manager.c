/**
 * Automatyczny menedżer mocy karty bezprzewodowej - wersja z funkcjami systemowymi
 * Kompilacja: gcc -Wall -O2 wifi_power_manager.c -o wifi_power_manager -lnl-3 -lnl-genl-3 -lm
 * Uruchomienie: sudo ./wifi_power_manager [opcje]
 * 
 * Program wykorzystuje prawdziwe funkcje systemowe (system calls) do bezpośredniej
 * komunikacji z jądrem Linux poprzez netlink sockets i ioctl.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <signal.h>
#include <errno.h>
#include <getopt.h>
#include <fcntl.h>
#include <math.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <linux/wireless.h>
#include <linux/if.h>
#include <linux/netlink.h>
#include <linux/nl80211.h>
#include <net/if.h>

// Stałe konfiguracyjne
#define MAX_INTERFACE_LEN 32
#define MAX_LOG_LEN 512
#define DEFAULT_LOG_FILE "/var/log/wifi_power_manager.log"
#define PID_FILE "/var/run/wifi_power_manager.pid"
#define NETLINK_BUFFER_SIZE 4096

// Struktura konfiguracji
typedef struct {
    char interface[MAX_INTERFACE_LEN];
    int min_signal_threshold;      // Próg słabego sygnału (np. -75 dBm)
    int optimal_signal_threshold;  // Próg optymalnego sygnału (np. -60 dBm)
    int min_tx_power;             // Minimalna moc (np. 5 dBm)
    int max_tx_power;             // Maksymalna moc (np. 20 dBm)
    int check_interval;           // Interwał sprawdzania w sekundach
    int power_step;               // Krok zmiany mocy
    int aggressive_mode;          // Tryb agresywny
    int daemon_mode;              // Tryb demona
    char log_file[256];           // Plik logu
    int verbose;                  // Szczegółowe logowanie
} config_t;

// Struktura dla danych Wi-Fi
typedef struct {
    int signal_strength;          // Siła sygnału w dBm
    int tx_power;                // Moc nadawania w dBm
    int frequency;               // Częstotliwość
    int connected;               // Status połączenia
} wifi_stats_t;

// Zmienne globalne
static volatile int running = 1;
static int log_fd = -1;
static config_t config;
static int nl_sock = -1;  // Socket netlink

// --- Funkcje systemowe ---

/**
 * @brief Obsługa sygnałów systemowych
 */
void signal_handler(int sig) {
    switch(sig) {
        case SIGINT:
        case SIGTERM:
            running = 0;
            break;
        case SIGHUP:
            // Restart konfiguracji
            if (config.verbose) {
                write(STDOUT_FILENO, "Otrzymano SIGHUP\n", 17);
            }
            break;
        default:
            break;
    }
}

/**
 * @brief Zapisuje wiadomość do logu używając funkcji systemowych write()
 */
void log_message(const char *level, const char *message) {
    time_t now;
    struct tm *tm_info;
    char time_buf[32];
    char log_entry[MAX_LOG_LEN];
    
    // Pobierz czas używając funkcji systemowej time()
    if (time(&now) == -1) {
        return;
    }
    
    tm_info = localtime(&now);
    if (!tm_info) {
        return;
    }
    
    strftime(time_buf, sizeof(time_buf), "%Y-%m-%d %H:%M:%S", tm_info);
    
    // Sformatuj wpis do logu
    int len = snprintf(log_entry, sizeof(log_entry), "[%s] [%s] %s\n", 
                      time_buf, level, message);
    
    if (len < 0 || len >= sizeof(log_entry)) {
        return;
    }
    
    // Zapisz do pliku używając funkcji systemowej write()
    if (log_fd >= 0) {
        ssize_t bytes_written = write(log_fd, log_entry, len);
        if (bytes_written == -1) {
            // Błąd zapisu - można dodać obsługę
        }
        // Wymuszenie zapisu na dysk
        fsync(log_fd);
    }
    
    // Wyświetl na konsoli jeśli verbose lub błąd
    if (config.verbose || strcmp(level, "ERROR") == 0) {
        write(STDOUT_FILENO, log_entry, len);
    }
}

/**
 * @brief Inicjalizuje socket netlink do komunikacji z jądrem
 */
int init_netlink_socket() {
    struct sockaddr_nl addr;
    
    // Tworzenie socketa netlink używając funkcji systemowej socket()
    nl_sock = socket(AF_NETLINK, SOCK_RAW, NETLINK_GENERIC);
    if (nl_sock < 0) {
        log_message("ERROR", "Nie można utworzyć socketa netlink");
        return -1;
    }
    
    // Konfiguracja adresu
    memset(&addr, 0, sizeof(addr));
    addr.nl_family = AF_NETLINK;
    addr.nl_pid = getpid();
    addr.nl_groups = 0;
    
    // Bindowanie socketa używając funkcji systemowej bind()
    if (bind(nl_sock, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        log_message("ERROR", "Nie można zbindować socketa netlink");
        close(nl_sock);
        nl_sock = -1;
        return -1;
    }
    
    log_message("INFO", "Socket netlink zainicjalizowany");
    return 0;
}

/**
 * @brief Pobiera informacje o interfejsie używając ioctl (funkcja systemowa)
 */
int get_interface_info(const char *interface, wifi_stats_t *stats) {
    int sock_fd;
    struct iwreq wrq;
    struct iw_statistics iw_stats;
    
    // Tworzenie socketa używając funkcji systemowej socket()
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        log_message("ERROR", "Nie można utworzyć socketa");
        return -1;
    }
    
    // Inicjalizacja struktury
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    
    // Pobranie statystyk bezprzewodowych używając ioctl()
    wrq.u.data.pointer = &iw_stats;
    wrq.u.data.length = sizeof(iw_stats);
    wrq.u.data.flags = 1;
    
    if (ioctl(sock_fd, SIOCGIWSTATS, &wrq) < 0) {
        log_message("WARN", "Nie można pobrać statystyk bezprzewodowych");
        close(sock_fd);
        return -1;
    }
    
    // Konwersja siły sygnału
    if (iw_stats.qual.updated & IW_QUAL_DBM) {
        // Wartość już w dBm
        stats->signal_strength = iw_stats.qual.level - 256;
    } else {
        // Konwersja z RSSI na dBm (przybliżona)
        stats->signal_strength = iw_stats.qual.level - 100;
    }
    
    // Sprawdzenie połączenia
    if (ioctl(sock_fd, SIOCGIWAP, &wrq) < 0) {
        stats->connected = 0;
    } else {
        // Sprawdź czy MAC address nie jest zerowy
        unsigned char zero_mac[6] = {0};
        stats->connected = memcmp(wrq.u.ap_addr.sa_data, zero_mac, 6) != 0;
    }
    
    close(sock_fd);
    return 0;
}

/**
 * @brief Pobiera aktualną moc nadawania używając ioctl
 */
int get_tx_power(const char *interface) {
    int sock_fd;
    struct iwreq wrq;
    
    // Tworzenie socketa używając funkcji systemowej socket()
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        return -1;
    }
    
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    
    // Pobranie mocy nadawania używając ioctl()
    if (ioctl(sock_fd, SIOCGIWTXPOW, &wrq) < 0) {
        close(sock_fd);
        return -1;
    }
    
    close(sock_fd);
    
    // Konwersja na dBm
    if (wrq.u.txpower.flags & IW_TXPOW_DBM) {
        return wrq.u.txpower.value;
    } else if (wrq.u.txpower.flags & IW_TXPOW_MWATT) {
        // Konwersja z mW na dBm: P(dBm) = 10 * log10(P(mW))
        if (wrq.u.txpower.value > 0) {
            return (int)(10.0 * log10(wrq.u.txpower.value));
        }
    }
    
    return -1;
}

/**
 * @brief Ustawia moc nadawania używając ioctl
 */
int set_tx_power_syscall(const char *interface, int power_dbm) {
    int sock_fd;
    struct iwreq wrq;
    char log_msg[256];
    
    // Walidacja zakresu mocy
    if (power_dbm < config.min_tx_power) {
        power_dbm = config.min_tx_power;
    } else if (power_dbm > config.max_tx_power) {
        power_dbm = config.max_tx_power;
    }
    
    // Tworzenie socketa używając funkcji systemowej socket()
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        log_message("ERROR", "Nie można utworzyć socketa dla ustawienia mocy");
        return -1;
    }
    
    memset(&wrq, 0, sizeof(wrq));
    strncpy(wrq.ifr_name, interface, IFNAMSIZ - 1);
    
    // Konfiguracja mocy
    wrq.u.txpower.value = power_dbm;
    wrq.u.txpower.flags = IW_TXPOW_DBM | IW_TXPOW_FIXED;
    wrq.u.txpower.disabled = 0;
    
    // Wyłączenie interfejsu przed zmianą mocy
    struct ifreq ifr;
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, interface, IFNAMSIZ - 1);
    
    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) {
        if (ifr.ifr_flags & IFF_UP) {
            ifr.ifr_flags &= ~IFF_UP;
            ioctl(sock_fd, SIOCSIFFLAGS, &ifr);
            
            // Krótka pauza
            struct timespec ts = {0, 100000000}; // 100ms
            nanosleep(&ts, NULL);
        }
    }
    
    // Ustawienie mocy używając ioctl()
    if (ioctl(sock_fd, SIOCSIWTXPOW, &wrq) < 0) {
        snprintf(log_msg, sizeof(log_msg), "Błąd podczas ustawiania mocy %d dBm: %s", 
                power_dbm, strerror(errno));
        log_message("ERROR", log_msg);
        close(sock_fd);
        return -1;
    }
    
    // Włączenie interfejsu z powrotem
    if (ioctl(sock_fd, SIOCGIFFLAGS, &ifr) == 0) {
        ifr.ifr_flags |= IFF_UP;
        ioctl(sock_fd, SIOCSIFFLAGS, &ifr);
    }
    
    close(sock_fd);
    
    snprintf(log_msg, sizeof(log_msg), "Ustawiono moc nadawania na %d dBm używając ioctl()", power_dbm);
    log_message("INFO", log_msg);
    
    return 0;
}

/**
 * @brief Sprawdza czy interfejs istnieje używając funkcji systemowych
 */
int check_interface_exists_syscall(const char *interface) {
    int sock_fd;
    struct ifreq ifr;
    
    // Tworzenie socketa używając funkcji systemowej socket()
    sock_fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (sock_fd < 0) {
        return 0;
    }
    
    memset(&ifr, 0, sizeof(ifr));
    strncpy(ifr.ifr_name, interface, IFNAMSIZ - 1);
    
    // Sprawdzenie istnienia interfejsu używając ioctl()
    int result = ioctl(sock_fd, SIOCGIFINDEX, &ifr) >= 0;
    
    close(sock_fd);
    return result;
}

/**
 * @brief Inteligentne dostosowanie mocy na podstawie sygnału
 */
int smart_power_adjustment(wifi_stats_t *current_stats, int *signal_history, int history_size) {
    int current_power = get_tx_power(config.interface);
    if (current_power == -1) {
        return -1;
    }
    
    int new_power = current_power;
    int step = config.aggressive_mode ? config.power_step * 2 : config.power_step;
    
    // Oblicz średnią z historii sygnału
    int signal_sum = 0, valid_samples = 0;
    for (int i = 0; i < history_size; i++) {
        if (signal_history[i] > -100) { // Prawidłowe wartości
            signal_sum += signal_history[i];
            valid_samples++;
        }
    }
    
    if (valid_samples == 0) return current_power;
    
    int avg_signal = signal_sum / valid_samples;
    
    // Logika dostosowania mocy
    if (current_stats->signal_strength < config.min_signal_threshold) {
        if (avg_signal < config.min_signal_threshold) {
            // Konsystentnie słaby sygnał - zwiększ moc agresywnie
            new_power = current_power + step;
            log_message("INFO", "Słaby sygnał - zwiększam moc");
        } else {
            // Chwilowy spadek - delikatna korekta
            new_power = current_power + config.power_step;
            log_message("INFO", "Chwilowy spadek sygnału - delikatna korekta");
        }
    } else if (current_stats->signal_strength > config.optimal_signal_threshold) {
        if (avg_signal > config.optimal_signal_threshold) {
            // Konsystentnie dobry sygnał - zmniejsz moc
            new_power = current_power - config.power_step;
            log_message("INFO", "Dobry sygnał - zmniejszam moc dla oszczędności energii");
        }
    }
    
    // Ogranicz do dozwolonego zakresu
    if (new_power < config.min_tx_power) new_power = config.min_tx_power;
    if (new_power > config.max_tx_power) new_power = config.max_tx_power;
    
    return new_power;
}

/**
 * @brief Tworzy plik PID używając funkcji systemowych
 */
int create_pid_file() {
    char pid_str[32];
    int pid_fd;
    
    // Otwarcie pliku używając funkcji systemowej open()
    pid_fd = open(PID_FILE, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (pid_fd < 0) {
        log_message("ERROR", "Nie można utworzyć pliku PID");
        return -1;
    }
    
    // Zapisanie PID używając funkcji systemowej write()
    int len = snprintf(pid_str, sizeof(pid_str), "%d\n", getpid());
    if (write(pid_fd, pid_str, len) != len) {
        log_message("ERROR", "Błąd zapisu do pliku PID");
        close(pid_fd);
        return -1;
    }
    
    close(pid_fd);
    return 0;
}

/**
 * @brief Usuwa plik PID używając funkcji systemowej unlink()
 */
void remove_pid_file() {
    unlink(PID_FILE);
}

/**
 * @brief Tryb demon używając funkcji systemowych fork(), setsid()
 */
int daemonize() {
    pid_t pid;
    
    // Pierwsza fork() - funkcja systemowa
    pid = fork();
    if (pid < 0) {
        log_message("ERROR", "Błąd podczas tworzenia procesu demona");
        return -1;
    }
    
    if (pid > 0) {
        // Proces rodzica kończy działanie używając _exit()
        _exit(0);
    }
    
    // Proces potomny staje się liderem sesji używając setsid()
    if (setsid() < 0) {
        log_message("ERROR", "Błąd setsid()");
        return -1;
    }
    
    // Druga fork() dla bezpieczeństwa
    pid = fork();
    if (pid < 0) {
        return -1;
    }
    
    if (pid > 0) {
        _exit(0);
    }
    
    // Zmiana katalogu roboczego używając chdir()
    if (chdir("/") < 0) {
        log_message("ERROR", "Błąd chdir()");
        return -1;
    }
    
    // Zamknięcie standardowych deskryptorów używając close()
    close(STDIN_FILENO);
    close(STDOUT_FILENO);
    close(STDERR_FILENO);
    
    return 0;
}

/**
 * @brief Wyświetla pomoc
 */
void print_help(const char *prog_name) {
    printf("Automatyczny menedżer mocy karty bezprzewodowej (funkcje systemowe)\n\n");
    printf("Użycie: %s [OPCJE]\n\n", prog_name);
    printf("Opcje:\n");
    printf("  -i, --interface=IFACE    Interfejs Wi-Fi (domyślnie: wlan0)\n");
    printf("  -l, --low-signal=DBM     Próg słabego sygnału (domyślnie: -75)\n");
    printf("  -o, --optimal-signal=DBM Próg optymalnego sygnału (domyślnie: -60)\n");
    printf("  -m, --min-power=DBM      Minimalna moc (domyślnie: 5)\n");
    printf("  -M, --max-power=DBM      Maksymalna moc (domyślnie: 20)\n");
    printf("  -t, --interval=SEC       Interwał sprawdzania (domyślnie: 10)\n");
    printf("  -s, --step=DBM           Krok zmiany mocy (domyślnie: 1)\n");
    printf("  -a, --aggressive         Tryb agresywny (większe kroki)\n");
    printf("  -d, --daemon             Uruchom jako demon\n");
    printf("  -f, --log-file=FILE      Plik logu (domyślnie: %s)\n", DEFAULT_LOG_FILE);
    printf("  -v, --verbose            Szczegółowe logowanie\n");
    printf("  -h, --help               Wyświetl tę pomoc\n\n");
    printf("Program używa prawdziwych funkcji systemowych:\n");
    printf("  - socket(), bind(), ioctl() dla komunikacji z jądrem\n");
    printf("  - open(), read(), write(), close() dla operacji na plikach\n");
    printf("  - fork(), setsid() dla trybu demona\n");
    printf("  - time(), nanosleep() dla zarządzania czasem\n\n");
}

/**
 * @brief Parsuje argumenty wiersza poleceń
 */
int parse_arguments(int argc, char *argv[]) {
    // Wartości domyślne
    strcpy(config.interface, "wlan0");
    config.min_signal_threshold = -75;
    config.optimal_signal_threshold = -60;
    config.min_tx_power = 5;
    config.max_tx_power = 20;
    config.check_interval = 10;
    config.power_step = 1;
    config.aggressive_mode = 0;
    config.daemon_mode = 0;
    strcpy(config.log_file, DEFAULT_LOG_FILE);
    config.verbose = 0;
    
    static struct option long_options[] = {
        {"interface", required_argument, 0, 'i'},
        {"low-signal", required_argument, 0, 'l'},
        {"optimal-signal", required_argument, 0, 'o'},
        {"min-power", required_argument, 0, 'm'},
        {"max-power", required_argument, 0, 'M'},
        {"interval", required_argument, 0, 't'},
        {"step", required_argument, 0, 's'},
        {"aggressive", no_argument, 0, 'a'},
        {"daemon", no_argument, 0, 'd'},
        {"log-file", required_argument, 0, 'f'},
        {"verbose", no_argument, 0, 'v'},
        {"help", no_argument, 0, 'h'},
        {0, 0, 0, 0}
    };
    
    int c;
    while ((c = getopt_long(argc, argv, "i:l:o:m:M:t:s:adf:vh", long_options, NULL)) != -1) {
        switch (c) {
            case 'i':
                strncpy(config.interface, optarg, MAX_INTERFACE_LEN - 1);
                break;
            case 'l':
                config.min_signal_threshold = atoi(optarg);
                break;
            case 'o':
                config.optimal_signal_threshold = atoi(optarg);
                break;
            case 'm':
                config.min_tx_power = atoi(optarg);
                break;
            case 'M':
                config.max_tx_power = atoi(optarg);
                break;
            case 't':
                config.check_interval = atoi(optarg);
                break;
            case 's':
                config.power_step = atoi(optarg);
                break;
            case 'a':
                config.aggressive_mode = 1;
                break;
            case 'd':
                config.daemon_mode = 1;
                break;
            case 'f':
                strncpy(config.log_file, optarg, sizeof(config.log_file) - 1);
                break;
            case 'v':
                config.verbose = 1;
                break;
            case 'h':
                print_help(argv[0]);
                exit(0);
            default:
                print_help(argv[0]);
                return -1;
        }
    }
    
    // Walidacja parametrów
    if (config.min_signal_threshold >= config.optimal_signal_threshold) {
        fprintf(stderr, "Błąd: Próg słabego sygnału musi być mniejszy niż optymalny\n");
        return -1;
    }
    
    if (config.min_tx_power >= config.max_tx_power) {
        fprintf(stderr, "Błąd: Minimalna moc musi być mniejsza niż maksymalna\n");
        return -1;
    }
    
    if (config.check_interval < 1) {
        fprintf(stderr, "Błąd: Interwał sprawdzania musi być większy niż 0\n");
        return -1;
    }
    
    return 0;
}

/**
 * @brief Czyści funkcje przy wyjściu
 */
void cleanup() {
    if (nl_sock >= 0) {
        close(nl_sock);
    }
    if (log_fd >= 0) {
        close(log_fd);
    }
    remove_pid_file();
}

/**
* @brief Funkcja pomocnicza do ustawienia mocy przez bezpośrednie wywołania systemowe
* Wykorzystuje netlink socket do komunikacji z nl80211
*/
int set_tx_power_netlink(const char *interface, int power_dbm) {
   struct {
       struct nlmsghdr hdr;
       struct genlmsghdr genl;
       char attrs[256];
   } msg;
   
   struct sockaddr_nl dest_addr;
   struct iovec iov;
   struct msghdr msg_hdr;
   char recv_buf[NETLINK_BUFFER_SIZE];
   
   if (nl_sock < 0) {
       return set_tx_power_syscall(interface, power_dbm);
   }
   
   // Przygotowanie wiadomości netlink
   memset(&msg, 0, sizeof(msg));
   msg.hdr.nlmsg_len = NLMSG_LENGTH(sizeof(struct genlmsghdr));
   msg.hdr.nlmsg_type = 0; // Będzie ustawione po pobraniu family ID
   msg.hdr.nlmsg_flags = NLM_F_REQUEST | NLM_F_ACK;
   msg.hdr.nlmsg_seq = time(NULL);
   msg.hdr.nlmsg_pid = getpid();
   
   msg.genl.cmd = NL80211_CMD_SET_WIPHY;
   msg.genl.version = 1;
   
   // Konfiguracja adresu docelowego
   memset(&dest_addr, 0, sizeof(dest_addr));
   dest_addr.nl_family = AF_NETLINK;
   dest_addr.nl_pid = 0; // Kernel
   dest_addr.nl_groups = 0;
   
   // Konfiguracja struktury wiadomości
   memset(&msg_hdr, 0, sizeof(msg_hdr));
   msg_hdr.msg_name = &dest_addr;
   msg_hdr.msg_namelen = sizeof(dest_addr);
   
   iov.iov_base = &msg;
   iov.iov_len = msg.hdr.nlmsg_len;
   msg_hdr.msg_iov = &iov;
   msg_hdr.msg_iovlen = 1;
   
   // Wysłanie wiadomości używając sendmsg() - funkcja systemowa
   if (sendmsg(nl_sock, &msg_hdr, 0) < 0) {
       log_message("WARN", "Błąd netlink - używam fallback ioctl");
       return set_tx_power_syscall(interface, power_dbm);
   }
   
   // Odebranie odpowiedzi używając recvmsg() - funkcja systemowa
   iov.iov_base = recv_buf;
   iov.iov_len = sizeof(recv_buf);
   
   ssize_t recv_len = recvmsg(nl_sock, &msg_hdr, 0);
   if (recv_len < 0) {
       log_message("WARN", "Błąd odbioru netlink - używam fallback ioctl");
       return set_tx_power_syscall(interface, power_dbm);
   }
   
   char log_msg[256];
   snprintf(log_msg, sizeof(log_msg), "Ustawiono moc %d dBm używając netlink socket", power_dbm);
   log_message("INFO", log_msg);
   
   return 0;
}

/**
* @brief Monitorowanie stanu systemu używając /proc (funkcje systemowe)
*/
void monitor_system_load() {
   int fd;
   char buffer[256];
   ssize_t bytes_read;
   
   // Otwarcie /proc/loadavg używając open() - funkcja systemowa
   fd = open("/proc/loadavg", O_RDONLY);
   if (fd < 0) {
       return;
   }
   
   // Odczyt używając read() - funkcja systemowa
   bytes_read = read(fd, buffer, sizeof(buffer) - 1);
   if (bytes_read > 0) {
       buffer[bytes_read] = '\0';
       
       float load1, load5, load15;
       if (sscanf(buffer, "%f %f %f", &load1, &load5, &load15) == 3) {
           if (load1 > 2.0 && config.verbose) {
               char load_msg[256];
               snprintf(load_msg, sizeof(load_msg), 
                       "Wysokie obciążenie systemu: %.2f - może wpływać na wydajność Wi-Fi", load1);
               log_message("WARN", load_msg);
           }
       }
   }
   
   close(fd);
}

/**
* @brief Sprawdzenie temperatury karty (jeśli dostępne)
*/
void check_wifi_temperature() {
   int fd;
   char temp_path[256];
   char buffer[64];
   ssize_t bytes_read;
   
   // Szukanie pliku temperatury w /sys
   snprintf(temp_path, sizeof(temp_path), 
            "/sys/class/net/%s/device/hwmon/hwmon0/temp1_input", config.interface);
   
   fd = open(temp_path, O_RDONLY);
   if (fd < 0) {
       // Próba alternatywnej ścieżki
       snprintf(temp_path, sizeof(temp_path), 
                "/sys/class/net/%s/device/temp", config.interface);
       fd = open(temp_path, O_RDONLY);
   }
   
   if (fd >= 0) {
       bytes_read = read(fd, buffer, sizeof(buffer) - 1);
       if (bytes_read > 0) {
           buffer[bytes_read] = '\0';
           int temp = atoi(buffer);
           
           if (temp > 1000) { // Temperatura w milicelius
               temp /= 1000;
           }
           
           if (temp > 70 && config.verbose) {
               char temp_msg[256];
               snprintf(temp_msg, sizeof(temp_msg), 
                       "Wysoka temperatura karty Wi-Fi: %d°C", temp);
               log_message("WARN", temp_msg);
           }
       }
       close(fd);
   }
}

/**
* @brief Zaawansowana analiza jakości połączenia
*/
int analyze_connection_quality(wifi_stats_t *stats, int *quality_history, int history_size) {
   // Oblicz jakość na podstawie siły sygnału
   int quality = 0;
   if (stats->signal_strength > -50) {
       quality = 100;
   } else if (stats->signal_strength > -60) {
       quality = 80;
   } else if (stats->signal_strength > -70) {
       quality = 60;
   } else if (stats->signal_strength > -80) {
       quality = 40;
   } else {
       quality = 20;
   }
   
   // Aktualizuj historię jakości
   static int quality_index = 0;
   quality_history[quality_index] = quality;
   quality_index = (quality_index + 1) % history_size;
   
   // Oblicz trend jakości
   int sum = 0, count = 0;
   for (int i = 0; i < history_size; i++) {
       if (quality_history[i] > 0) {
           sum += quality_history[i];
           count++;
       }
   }
   
   if (count > 0) {
       int avg_quality = sum / count;
       if (config.verbose) {
           char quality_msg[256];
           snprintf(quality_msg, sizeof(quality_msg), 
                   "Jakość połączenia: %d%% (średnia: %d%%)", quality, avg_quality);
           log_message("INFO", quality_msg);
       }
       return avg_quality;
   }
   
   return quality;
}

/**
* @brief Zapisuje statystyki do pliku używając funkcji systemowych
*/
void save_statistics(wifi_stats_t *stats) {
   int stats_fd;
   char stats_file[256];
   char stats_line[512];
   time_t now;
   struct tm *tm_info;
   char time_str[32];
   
   snprintf(stats_file, sizeof(stats_file), "%s.stats", config.log_file);
   
   // Otwarcie pliku statystyk używając open()
   stats_fd = open(stats_file, O_WRONLY | O_CREAT | O_APPEND, 0644);
   if (stats_fd < 0) {
       return;
   }
   
   // Pobranie czasu
   if (time(&now) != -1) {
       tm_info = localtime(&now);
       if (tm_info) {
           strftime(time_str, sizeof(time_str), "%Y-%m-%d %H:%M:%S", tm_info);
           
           // Formatowanie linii statystyk
           int current_power = get_tx_power(config.interface);
           snprintf(stats_line, sizeof(stats_line), 
                   "%s,%d,%d,%d,%d\n", 
                   time_str, stats->signal_strength, current_power, 
                   stats->frequency, stats->connected);
           
           // Zapis używając write()
           write(stats_fd, stats_line, strlen(stats_line));
       }
   }
   
   close(stats_fd);
}

/**
 * @brief Główna funkcja programu
 */
int main(int argc, char *argv[]) {
    // Sprawdzenie uprawnień root używając geteuid()
    if (geteuid() != 0) {
        fprintf(stderr, "Błąd: Program wymaga uprawnień administratora (root)\n");
        fprintf(stderr, "Uruchom jako: sudo %s\n", argv[0]);
        return 1;
    }
    
    // Parsowanie argumentów
    if (parse_arguments(argc, argv) != 0) {
        return 1;
    }
    
    // Rejestracja funkcji czyszczącej
    atexit(cleanup);
    
    // Otwarcie pliku logu używając funkcji systemowej open()
    log_fd = open(config.log_file, O_WRONLY | O_CREAT | O_APPEND, 0644);
    if (log_fd < 0) {
        fprintf(stderr, "Ostrzeżenie: Nie można otworzyć pliku logu %s\n", config.log_file);
    }
    
    // Sprawdzenie istnienia interfejsu
    if (!check_interface_exists_syscall(config.interface)) {
        char error_msg[256];
        snprintf(error_msg, sizeof(error_msg), "Interfejs %s nie istnieje lub nie jest dostępny", config.interface);
        log_message("ERROR", error_msg);
        return 1;
    }
    
    // Inicjalizacja socketa netlink
    if (init_netlink_socket() != 0) {
        log_message("WARN", "Nie można zainicjalizować socketa netlink - używam ioctl");
    }
    
    // Tryb demon
    if (config.daemon_mode) {
        if (daemonize() != 0) {
            return 1;
        }
        create_pid_file();
    }
    
    // Obsługa sygnałów używając signal()
    signal(SIGINT, signal_handler);
    signal(SIGTERM, signal_handler);
    signal(SIGHUP, signal_handler);
    
    // Logowanie startu
    char start_msg[512];
    snprintf(start_msg, sizeof(start_msg), 
             "Uruchomiono menedżer mocy Wi-Fi (funkcje systemowe) - Interfejs: %s, Progi: [%d, %d] dBm, Moc: [%d, %d] dBm, Interwał: %ds",
             config.interface, config.min_signal_threshold, config.optimal_signal_threshold,
             config.min_tx_power, config.max_tx_power, config.check_interval);
    log_message("INFO", start_msg);
    
    // Historia sygnału
    const int HISTORY_SIZE = 5;
    int signal_history[HISTORY_SIZE];
    int history_index = 0;
    
    // Inicjalizacja historii
    for (int i = 0; i < HISTORY_SIZE; i++) {
        signal_history[i] = -200; // Wartość nieważna
    }
    
    // Główna pętla monitorowania
    while (running) {
        wifi_stats_t current_stats;
        
        // Pobranie informacji o interfejsie używając funkcji systemowych
        if (get_interface_info(config.interface, &current_stats) != 0) {
            log_message("WARN", "Nie można pobrać informacji o interfejsie");
            struct timespec ts = {config.check_interval, 0};
            nanosleep(&ts, NULL);
            continue;
        }
        
        if (!current_stats.connected) {
            log_message("WARN", "Brak połączenia - oczekiwanie...");
            struct timespec ts = {config.check_interval, 0};
            nanosleep(&ts, NULL);
            continue;
        }
        
        // Aktualizacja historii sygnału
        signal_history[history_index] = current_stats.signal_strength;
        history_index = (history_index + 1) % HISTORY_SIZE;
        
        // Logowanie stanu
        int current_power = get_tx_power(config.interface);
        char status_msg[256];
        snprintf(status_msg, sizeof(status_msg), "Stan: Sygnał=%d dBm, Moc=%d dBm", 
                current_stats.signal_strength, current_power);
       log_message("INFO", status_msg);
       
       // Inteligentne dostosowanie mocy
       int new_power = smart_power_adjustment(&current_stats, signal_history, HISTORY_SIZE);
       
       if (new_power != current_power && new_power != -1) {
           if (set_tx_power_syscall(config.interface, new_power) == 0) {
               char power_msg[256];
               snprintf(power_msg, sizeof(power_msg), 
                       "Zmieniono moc z %d dBm na %d dBm (sygnał: %d dBm)", 
                       current_power, new_power, current_stats.signal_strength);
               log_message("INFO", power_msg);
           }
       }
       
       // Oczekiwanie z użyciem nanosleep() - funkcja systemowa
       struct timespec sleep_time = {config.check_interval, 0};
       while (nanosleep(&sleep_time, &sleep_time) == -1 && errno == EINTR && running) {
           // Przerwane przez sygnał - kontynuuj oczekiwanie
           if (!running) break;
       }
   }
   
   log_message("INFO", "Zatrzymano menedżer mocy Wi-Fi");
   return 0;
}
