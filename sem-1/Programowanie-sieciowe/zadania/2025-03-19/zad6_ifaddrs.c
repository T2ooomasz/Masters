/*
 * Program demonstrujący wykorzystanie funkcji systemowej getifaddrs(3)
 * do pobierania informacji o interfejsach sieciowych
 *
 * Compilation: gcc -Wall ./ifaddrs.c -o ./ifaddrs
 * Usage:       ./ifaddrs
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ifaddrs.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netpacket/packet.h>
#include <net/if.h>

void print_interface_info(struct ifaddrs *ifa) {
    printf("Interface: %s\n", ifa->ifa_name);
    
    // Wyświetlanie flag interfejsu
    printf("  Flags: ");
    if (ifa->ifa_flags & IFF_UP) printf("UP ");
    if (ifa->ifa_flags & IFF_BROADCAST) printf("BROADCAST ");
    if (ifa->ifa_flags & IFF_DEBUG) printf("DEBUG ");
    if (ifa->ifa_flags & IFF_LOOPBACK) printf("LOOPBACK ");
    if (ifa->ifa_flags & IFF_POINTOPOINT) printf("POINTOPOINT ");
    if (ifa->ifa_flags & IFF_RUNNING) printf("RUNNING ");
    if (ifa->ifa_flags & IFF_NOARP) printf("NOARP ");
    if (ifa->ifa_flags & IFF_PROMISC) printf("PROMISC ");
    if (ifa->ifa_flags & IFF_MULTICAST) printf("MULTICAST ");
    printf("\n");
    
    // Sprawdzanie rodzaju adresu
    if (ifa->ifa_addr != NULL) {
        switch (ifa->ifa_addr->sa_family) {
            case AF_INET: {
                struct sockaddr_in *addr_in = (struct sockaddr_in *)ifa->ifa_addr;
                printf("  IPv4 Address: %s\n", inet_ntoa(addr_in->sin_addr));
                
                // Wyświetlanie maski sieciowej jeśli dostępna
                if (ifa->ifa_netmask != NULL) {
                    struct sockaddr_in *netmask_in = (struct sockaddr_in *)ifa->ifa_netmask;
                    printf("  Netmask: %s\n", inet_ntoa(netmask_in->sin_addr));
                }
                
                // Wyświetlanie adresu broadcast jeśli dostępny
                if (ifa->ifa_broadaddr != NULL && (ifa->ifa_flags & IFF_BROADCAST)) {
                    struct sockaddr_in *broadcast_in = (struct sockaddr_in *)ifa->ifa_broadaddr;
                    printf("  Broadcast: %s\n", inet_ntoa(broadcast_in->sin_addr));
                }
                break;
            }
            case AF_INET6: {
                struct sockaddr_in6 *addr_in6 = (struct sockaddr_in6 *)ifa->ifa_addr;
                char addr_str[INET6_ADDRSTRLEN];
                inet_ntop(AF_INET6, &addr_in6->sin6_addr, addr_str, INET6_ADDRSTRLEN);
                printf("  IPv6 Address: %s\n", addr_str);
                break;
            }
            case AF_PACKET: {
                struct sockaddr_ll *addr_ll = (struct sockaddr_ll *)ifa->ifa_addr;
                if (addr_ll->sll_halen == 6) {  // Ethernet MAC
                    printf("  MAC Address: %02x:%02x:%02x:%02x:%02x:%02x\n",
                           addr_ll->sll_addr[0], addr_ll->sll_addr[1],
                           addr_ll->sll_addr[2], addr_ll->sll_addr[3],
                           addr_ll->sll_addr[4], addr_ll->sll_addr[5]);
                }
                break;
            }
            default:
                printf("  Address family: %d (other)\n", ifa->ifa_addr->sa_family);
                break;
        }
    }
    printf("\n");
}

int main(int argc, char **argv) {
    struct ifaddrs *ifaddrs_ptr, *ifa;
    
    printf("=== Informacje o interfejsach sieciowych (getifaddrs) ===\n\n");
    
    // Pobieranie listy interfejsów
    if (getifaddrs(&ifaddrs_ptr) == -1) {
        perror("getifaddrs");
        return EXIT_FAILURE;
    }
    
    // Iteracja przez wszystkie interfejsy
    for (ifa = ifaddrs_ptr; ifa != NULL; ifa = ifa->ifa_next) {
        print_interface_info(ifa);
    }
    
    // Zwolnienie pamięci
    freeifaddrs(ifaddrs_ptr);
    
    printf("=== Demonstracja funkcji getifaddrs(3) zakończona ===\n");
    
    return EXIT_SUCCESS;
}