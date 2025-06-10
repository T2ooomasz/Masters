#include <arpa/inet.h>
#include <errno.h>
#include <linux/if.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <linux/route.h>
#include <netinet/in.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

// Define the custom EtherType for our protocol
#define ETH_P_CUSTOM 0x8888

// Define message types from the task description
#define IRI_T_ADDRESS 0
#define IRI_T_ROUTE 1

// Structure for transmitting configuration data, as per the task description.
struct ifrtinfo {
  int iri_type;
  char iri_iname[IFNAMSIZ];
  struct sockaddr_in iri_iaddr; /* IP address */
  struct sockaddr_in iri_rtdst; /* dst. IP address */
  struct sockaddr_in iri_rtmsk; /* dst. netmask */
  struct sockaddr_in iri_rtgip; /* gateway IP */
};

/**
 * @brief Configures the IP address of a network interface and brings it up.
 *
 * @param iri A pointer to the received configuration data.
 * @return 0 on success, -1 on failure.
 */
int configure_address(const struct ifrtinfo* iri) {
  struct ifreq ifr;
  struct sockaddr_in* sin;
  int sfd;

  printf("--> Received ADDRESS configuration request for interface '%s'\n", iri->iri_iname);

  // Create a socket to use for ioctl calls
  sfd = socket(PF_INET, SOCK_DGRAM, 0);
  if (sfd == -1) {
    perror("socket(PF_INET, SOCK_DGRAM)");
    return -1;
  }

  // Copy the interface name safely
  strncpy(ifr.ifr_name, iri->iri_iname, IFNAMSIZ - 1);
  ifr.ifr_name[IFNAMSIZ - 1] = '\0';

  // Set the IP address
  sin = (struct sockaddr_in*)&ifr.ifr_addr;
  *sin = iri->iri_iaddr; // Struct copy
  printf("    Setting IP address: %s\n", inet_ntoa(sin->sin_addr));
  if (ioctl(sfd, SIOCSIFADDR, &ifr) == -1) {
    perror("ioctl(SIOCSIFADDR)");
    close(sfd);
    return -1;
  }

  // Get the current interface flags
  if (ioctl(sfd, SIOCGIFFLAGS, &ifr) == -1) {
    perror("ioctl(SIOCGIFFLAGS)");
    close(sfd);
    return -1;
  }

  // Set the interface "UP" and "RUNNING" flags
  ifr.ifr_flags |= (IFF_UP | IFF_RUNNING);
  if (ioctl(sfd, SIOCSIFFLAGS, &ifr) == -1) {
    perror("ioctl(SIOCSIFFLAGS)");
    close(sfd);
    return -1;
  }

  printf("    Interface '%s' is now up.\n", iri->iri_iname);
  close(sfd);
  return 0;
}

/**
 * @brief Adds a new entry to the system's kernel routing table.
 *
 * @param iri A pointer to the received configuration data.
 * @return 0 on success, -1 on failure.
 */
int configure_route(const struct ifrtinfo* iri) {
  struct rtentry route;
  int sfd;

  printf("--> Received ROUTE configuration request.\n");

  // Create a socket to use for ioctl calls
  sfd = socket(PF_INET, SOCK_DGRAM, 0);
  if (sfd == -1) {
    perror("socket(PF_INET, SOCK_DGRAM)");
    return -1;
  }

  // Zero out the route structure and populate it from the received data
  memset(&route, 0, sizeof(route));
  route.rt_gateway = *(struct sockaddr*)&iri->iri_rtgip;
  route.rt_dst = *(struct sockaddr*)&iri->iri_rtdst;
  route.rt_genmask = *(struct sockaddr*)&iri->iri_rtmsk;
  route.rt_flags = RTF_UP | RTF_GATEWAY;

  // inet_ntoa is not thread-safe, but acceptable in this single-threaded context.
  printf("    Destination: %s\n", inet_ntoa(((struct sockaddr_in*)&route.rt_dst)->sin_addr));
  printf("    Gateway:     %s\n", inet_ntoa(((struct sockaddr_in*)&route.rt_gateway)->sin_addr));
  printf("    Netmask:     %s\n", inet_ntoa(((struct sockaddr_in*)&route.rt_genmask)->sin_addr));

  // Add the route using ioctl
  if (ioctl(sfd, SIOCADDRT, &route) == -1) {
    perror("ioctl(SIOCADDRT)");
    close(sfd);
    return -1;
  }

  printf("    Route has been successfully added.\n");
  close(sfd);
  return 0;
}

int main(int argc, char** argv) {
  if (argc < 2) {
    fprintf(stderr, "Usage: %s <interface>\n", argv[0]);
    return EXIT_FAILURE;
  }
  const char* ifname = argv[1];

  int sfd;
  ssize_t n;
  char frame[ETH_FRAME_LEN];
  char* fdata;
  struct ifrtinfo* iri;
  struct sockaddr_ll sall;
  struct ifreq ifr;

  // 1. Create a raw socket to receive all packets of our custom EtherType
  sfd = socket(AF_PACKET, SOCK_RAW, htons(ETH_P_CUSTOM));
  if (sfd == -1) {
    perror("socket(AF_PACKET, SOCK_RAW)");
    return EXIT_FAILURE;
  }

  // 2. Get the index of the interface to listen on
  strncpy(ifr.ifr_name, ifname, IFNAMSIZ - 1);
  ifr.ifr_name[IFNAMSIZ - 1] = '\0';
  if (ioctl(sfd, SIOCGIFINDEX, &ifr) == -1) {
    perror("ioctl(SIOCGIFINDEX)");
    close(sfd);
    return EXIT_FAILURE;
  }

  // 3. Bind the raw socket to the specified interface
  memset(&sall, 0, sizeof(sall));
  sall.sll_family = AF_PACKET;
  sall.sll_protocol = htons(ETH_P_CUSTOM);
  sall.sll_ifindex = ifr.ifr_ifindex;
  if (bind(sfd, (struct sockaddr*)&sall, sizeof(sall)) == -1) {
    perror("bind");
    close(sfd);
    return EXIT_FAILURE;
  }

  printf("Listening for frames with EtherType 0x%04X on interface %s...\n", ETH_P_CUSTOM, ifname);

  // 4. Main loop to receive and process frames
  while (1) {
    n = recvfrom(sfd, frame, ETH_FRAME_LEN, 0, NULL, NULL);
    if (n == -1) {
      perror("recvfrom");
      continue; // Try again on error
    }

    // The data payload (fdata) is located after the Ethernet header (14 bytes)
    fdata = frame + ETH_HLEN;
    iri = (struct ifrtinfo*)fdata;

    // Dispatch based on message type
    switch (iri->iri_type) {
      case IRI_T_ADDRESS:
        if (configure_address(iri) != 0) {
          fprintf(stderr, "Failed to configure IP address.\n");
        }
        break;

      case IRI_T_ROUTE:
        if (configure_route(iri) != 0) {
          fprintf(stderr, "Failed to configure route.\n");
        }
        break;

      default:
        fprintf(stderr, "Received unknown message type: %d\n", iri->iri_type);
        break;
    }
    printf("\nListening for next frame...\n");
  }

  // Cleanup (only reached if the loop is broken)
  close(sfd);
  return EXIT_SUCCESS;
}