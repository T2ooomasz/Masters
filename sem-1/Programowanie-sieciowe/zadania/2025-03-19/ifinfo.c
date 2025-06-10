/*
 * Copyright (C) 2020 Michal Kalewski <mkalewski at cs.put.poznan.pl>
 *
 * Compilation:  gcc -Wall ./ifinfo.c -o ./ifinfo
 * Usage:        ./ifinfo
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
#include <sys/socket.h>
#include <unistd.h>

struct ifconf getifreqs(int sfd) {
  int len, lastlen;
  char* buf;
  struct ifconf ifc;

  lastlen = 0;
  len = 100 * sizeof(struct ifreq);
  while(1) {
    buf = malloc(len);
    memset(buf, 0, len);
    ifc.ifc_len = len;
    ifc.ifc_buf = buf;
    ioctl(sfd, SIOCGIFCONF, &ifc);
    if (ifc.ifc_len == lastlen)
      break;
    lastlen = ifc.ifc_len;
    len += 10 * sizeof(struct ifreq);
    free(buf);
  }
  return ifc;
}

void ifsinfo(int sfd, struct ifconf ifc) {
  char* ptr;
  struct ifreq* ifr;
  struct sockaddr_in* addr_in;
  

  ptr = ifc.ifc_buf;
  while(ptr < ifc.ifc_buf + ifc.ifc_len) {
    ifr = (struct ifreq*) ptr;
    ptr += sizeof(struct ifreq);
    printf("%s\t", ifr->ifr_name);
    
    // Pobieranie adresu MAC interfejsu
    if (ioctl(sfd, SIOCGIFHWADDR, ifr) >= 0) {
      const unsigned char* mac = (unsigned char*)ifr->ifr_hwaddr.sa_data;
      printf("mac:%02X:%02X:%02X:%02X:%02X:%02X\t",
             mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]);
    } else {
      printf("mac:N/A\t");
    }
    
    // Pobieranie adresu IP interfejsu
    if (ioctl(sfd, SIOCGIFADDR, ifr) >= 0) {
      addr_in = (struct sockaddr_in*) &ifr->ifr_addr;
      printf("inet_addr:%s\n", inet_ntoa((struct in_addr)addr_in->sin_addr));
    } else {
      printf("inet_addr:N/A\n");
    }
  }
}

int main(int argc, char** argv) {
  int sfd = socket(PF_INET, SOCK_DGRAM, 0);
  if (sfd < 0) {
    perror("socket");
    return EXIT_FAILURE;
  }
  
  struct ifconf ifc = getifreqs(sfd);
  ifsinfo(sfd, ifc);
  free(ifc.ifc_buf);
  close(sfd);
  return EXIT_SUCCESS;
}