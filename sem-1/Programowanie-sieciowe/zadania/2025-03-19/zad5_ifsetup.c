/*
 * Copyright (C) 2020 Michal Kalewski <mkalewski at cs.put.poznan.pl>
 *
 * Compilation:  gcc -Wall ./ifsetup.c -o ./ifsetup
 * Usage:        ./ifsetup IFNAME IP NETMASK
 * NOTE:         This program requires root privileges.
 *
 * Bug reports:  https://gitlab.cs.put.poznan.pl/mkalewski/ps-2020/issues
 *
 */

#include <arpa/inet.h>
#include <linux/if.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

int main(int argc, char** argv) {
  int sfd;
  struct ifreq ifr;
  struct sockaddr_in* sin;

  // Sprawdzenie argumentów
  if (argc != 4) {
    printf("Usage: %s <interface_name> <ip_address> <netmask>\n", argv[0]);
    printf("Example: %s eth0 192.168.1.100 255.255.255.0\n", argv[0]);
    return EXIT_FAILURE;
  }

  sfd = socket(PF_INET, SOCK_DGRAM, 0);
  if (sfd < 0) {
    perror("socket");
    return EXIT_FAILURE;
  }
  
  // Przygotowanie struktury interfejsu
  memset(&ifr, 0, sizeof(ifr));
  strncpy(ifr.ifr_name, argv[1], IFNAMSIZ - 1);
  ifr.ifr_name[IFNAMSIZ - 1] = '\0';
  
  sin = (struct sockaddr_in*) &ifr.ifr_addr;
  memset(sin, 0, sizeof(struct sockaddr_in));
  sin->sin_family = AF_INET;
  sin->sin_port = 0;
  
  // Ustawianie adresu IP
  if (inet_pton(AF_INET, argv[2], &sin->sin_addr) <= 0) {
    printf("Błędny adres IP: %s\n", argv[2]);
    close(sfd);
    return EXIT_FAILURE;
  }
  
  if (ioctl(sfd, SIOCSIFADDR, &ifr) < 0) {
    perror("ioctl SIOCSIFADDR");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  // Ustawianie maski sieciowej
  sin = (struct sockaddr_in*) &ifr.ifr_netmask;
  memset(sin, 0, sizeof(struct sockaddr_in));
  sin->sin_family = AF_INET;
  sin->sin_port = 0;
  
  if (inet_pton(AF_INET, argv[3], &sin->sin_addr) <= 0) {
    printf("Błędna maska sieciowa: %s\n", argv[3]);
    close(sfd);
    return EXIT_FAILURE;
  }
  
  if (ioctl(sfd, SIOCSIFNETMASK, &ifr) < 0) {
    perror("ioctl SIOCSIFNETMASK");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  // Pobieranie aktualnych flag i włączanie interfejsu
  if (ioctl(sfd, SIOCGIFFLAGS, &ifr) < 0) {
    perror("ioctl SIOCGIFFLAGS");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  ifr.ifr_flags |= IFF_UP | IFF_RUNNING;
  
  if (ioctl(sfd, SIOCSIFFLAGS, &ifr) < 0) {
    perror("ioctl SIOCSIFFLAGS");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  printf("Interfejs %s skonfigurowany:\n", argv[1]);
  printf("  Adres IP: %s\n", argv[2]);
  printf("  Maska sieciowa: %s\n", argv[3]);
  printf("  Status: UP\n");
  
  close(sfd);
  return EXIT_SUCCESS;
}