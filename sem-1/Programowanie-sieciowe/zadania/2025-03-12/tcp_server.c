#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>
#include <netdb.h>

#define MAX 128

int main(int argc, char** argv) {
    socklen_t sl;
    int sfd, sfd2, cfd, on = 1;
    struct sockaddr_in saddr, saddr2, caddr;
    struct hostent* addrent;
    char buf[MAX];

    // Sprawdzenie argumentów
    if (argc != 3) {
        write(2, "Usage: program <hostname> <port>\n", 33);
        return EXIT_FAILURE;
    }

    // Konfiguracja adresu serwera (nasłuchiwanie na porcie 1234)
    memset(&saddr, 0, sizeof(saddr));
    saddr.sin_family = AF_INET;
    saddr.sin_port = htons(1234);
    saddr.sin_addr.s_addr = INADDR_ANY;

    // Konfiguracja adresu docelowego serwera (z argumentów)
    addrent = gethostbyname(argv[1]);
    if (addrent == NULL) {
        write(2, "Error: Cannot resolve hostname\n", 31);
        return EXIT_FAILURE;
    }
    
    memset(&saddr2, 0, sizeof(saddr2));
    saddr2.sin_family = AF_INET;
    saddr2.sin_port = htons(atoi(argv[2]));
    memcpy(&saddr2.sin_addr.s_addr, addrent->h_addr, addrent->h_length);
    
    // Utworzenie i konfiguracja gniazda serwera
    sfd = socket(PF_INET, SOCK_STREAM, 0);
    if (sfd < 0) {
        write(2, "Error: Cannot create socket\n", 28);
        return EXIT_FAILURE;
    }
    
    setsockopt(sfd, SOL_SOCKET, SO_REUSEADDR, (char*) &on, sizeof(on));
    
    if (bind(sfd, (struct sockaddr*) &saddr, sizeof(saddr)) < 0) {
        write(2, "Error: Cannot bind socket\n", 26);
        close(sfd);
        return EXIT_FAILURE;
    }
    
    if (listen(sfd, 5) < 0) {
        write(2, "Error: Cannot listen on socket\n", 31);
        close(sfd);
        return EXIT_FAILURE;
    }

    // Główna pętla serwera
    while(1) {
        // Wyzerowanie bufora
        memset(&buf, 0, MAX);
        memset(&caddr, 0, sizeof(caddr));
        sl = sizeof(caddr);
        
        // Akceptacja połączenia od klienta
        cfd = accept(sfd, (struct sockaddr*) &caddr, &sl);
        if (cfd < 0) {
            write(2, "Error: Cannot accept connection\n", 32);
            continue;
        }
        
        // Odbiór danych od klienta
        int bytes_read = read(cfd, buf, MAX);
        if (bytes_read <= 0) {
            close(cfd);
            continue;
        }
        
        // Rozłączenie z klientem
        close(cfd);
        
        // Utworzenie nowego gniazda do połączenia z docelowym serwerem
        sfd2 = socket(PF_INET, SOCK_STREAM, 0);
        if (sfd2 < 0) {
            write(2, "Error: Cannot create client socket\n", 35);
            continue;
        }
        
        // Nawiązanie połączenia z docelowym serwerem
        if (connect(sfd2, (struct sockaddr*) &saddr2, sizeof(saddr2)) < 0) {
            write(2, "Error: Cannot connect to target server\n", 40);
            close(sfd2);
            continue;
        }
        
        // Przesłanie danych do docelowego serwera
        write(sfd2, buf, bytes_read);
        
        // Zamknięcie połączenia z docelowym serwerem
        close(sfd2);
    }
    
    close(sfd);
    return EXIT_SUCCESS;
}