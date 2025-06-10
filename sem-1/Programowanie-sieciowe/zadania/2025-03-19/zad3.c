#include <linux/if.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <unistd.h>

int main(int argc, char** argv) {
  int sfd;
  struct ifreq ifr;
  
  // Sprawdzenie argumentów
  if (argc != 3) {
    printf("Usage: %s <interface_name> <0|1>\n", argv[0]);
    printf("  0 - wyłącz interfejs\n");
    printf("  1 - włącz interfejs\n");
    return EXIT_FAILURE;
  }
  
  sfd = socket(PF_INET, SOCK_DGRAM, 0);
  if (sfd < 0) {
    perror("socket");
    return EXIT_FAILURE;
  }
  
  // Kopiowanie nazwy interfejsu
  strncpy(ifr.ifr_name, argv[1], IFNAMSIZ - 1);
  ifr.ifr_name[IFNAMSIZ - 1] = '\0';
  
  // Pobieranie aktualnych flag interfejsu
  if (ioctl(sfd, SIOCGIFFLAGS, &ifr) < 0) {
    perror("ioctl SIOCGIFFLAGS");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  // Przetwarzanie argumentu włączenia/wyłączenia
  long onoff = strtol(argv[2], NULL, 10);
  if (onoff == 1) {
    ifr.ifr_flags |= IFF_UP;
    printf("Włączanie interfejsu %s\n", argv[1]);
  } else if (onoff == 0) {
    ifr.ifr_flags &= ~IFF_UP;
    printf("Wyłączanie interfejsu %s\n", argv[1]);
  } else {
    printf("Nieznane wywołanie programu - użyj 0 lub 1\n");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  // Ustawianie nowych flag interfejsu
  if (ioctl(sfd, SIOCSIFFLAGS, &ifr) < 0) {
    perror("ioctl SIOCSIFFLAGS");
    close(sfd);
    return EXIT_FAILURE;
  }
  
  printf("Operacja wykonana pomyślnie\n");
  close(sfd);
  return EXIT_SUCCESS;
}