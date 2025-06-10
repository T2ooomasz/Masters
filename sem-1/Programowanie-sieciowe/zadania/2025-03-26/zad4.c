#include <arpa/inet.h>
#include <linux/if_arp.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

#define ETH_P_CUSTOM 0x8888

int main(int argc, char** argv) {
    int sfd, cfd, interface_index;
    ssize_t len;
    char* frame;
    char* fdata;
    struct ethhdr* fhead;
    struct ifreq ifr;
    struct sockaddr_ll sall, sall2;
    
    // Check arguments
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <interface_name> <destination_mac>\n", argv[0]);
        fprintf(stderr, "Example: %s eth0 aa:bb:cc:dd:ee:ff\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    // Create sockets for receiving and sending
    sfd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_CUSTOM));  // receiving socket
    cfd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_CUSTOM));  // sending socket
    
    if (sfd < 0 || cfd < 0) {
        perror("socket");
        return EXIT_FAILURE;
    }
    
    // Initialize sockaddr_ll structures
    memset(&sall, 0, sizeof(struct sockaddr_ll));
    memset(&sall2, 0, sizeof(struct sockaddr_ll));
    
    // Get interface information
    strncpy(ifr.ifr_name, argv[1], IFNAMSIZ - 1);
    ifr.ifr_name[IFNAMSIZ - 1] = '\0';
    
    // Get interface index for both sockets
    if (ioctl(sfd, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl SIOCGIFINDEX on receiving socket");
        close(sfd);
        close(cfd);
        return EXIT_FAILURE;
    }
    interface_index = ifr.ifr_ifindex;
    
    if (ioctl(cfd, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl SIOCGIFINDEX on sending socket");
        close(sfd);
        close(cfd);
        return EXIT_FAILURE;
    }
    
    // Get interface hardware address (MAC)
    if (ioctl(cfd, SIOCGIFHWADDR, &ifr) < 0) {
        perror("ioctl SIOCGIFHWADDR");
        close(sfd);
        close(cfd);
        return EXIT_FAILURE;
    }
    
    // Configure receiving socket address
    sall.sll_family = AF_PACKET;
    sall.sll_protocol = htons(ETH_P_CUSTOM);
    sall.sll_ifindex = interface_index;
    sall.sll_hatype = ARPHRD_ETHER;
    sall.sll_pkttype = PACKET_HOST;
    sall.sll_halen = ETH_ALEN;
    
    // Configure sending socket address
    sall2.sll_family = AF_PACKET;
    sall2.sll_protocol = htons(ETH_P_CUSTOM);
    sall2.sll_ifindex = interface_index;
    sall2.sll_hatype = ARPHRD_ETHER;
    sall2.sll_pkttype = PACKET_OUTGOING;
    sall2.sll_halen = ETH_ALEN;
    
    // Parse destination MAC address from command line argument
    if (sscanf(argv[2], "%hhx:%hhx:%hhx:%hhx:%hhx:%hhx", 
               &sall2.sll_addr[0], &sall2.sll_addr[1], &sall2.sll_addr[2], 
               &sall2.sll_addr[3], &sall2.sll_addr[4], &sall2.sll_addr[5]) != 6) {
        fprintf(stderr, "Invalid MAC address format. Use: aa:bb:cc:dd:ee:ff\n");
        close(sfd);
        close(cfd);
        return EXIT_FAILURE;
    }
    
    // Bind receiving socket to interface
    if (bind(sfd, (struct sockaddr*) &sall, sizeof(struct sockaddr_ll)) < 0) {
        perror("bind");
        close(sfd);
        close(cfd);
        return EXIT_FAILURE;
    }
    
    printf("Frame forwarder started on interface %s\n", argv[1]);
    printf("Forwarding to MAC: %02x:%02x:%02x:%02x:%02x:%02x\n",
           sall2.sll_addr[0], sall2.sll_addr[1], sall2.sll_addr[2],
           sall2.sll_addr[3], sall2.sll_addr[4], sall2.sll_addr[5]);
    printf("Using EtherType: 0x%04x\n\n", ETH_P_CUSTOM);
    
    // Main forwarding loop
    while(1) {
        // Allocate memory for frame
        frame = malloc(ETH_FRAME_LEN);
        if (!frame) {
            perror("malloc");
            break;
        }
        memset(frame, 0, ETH_FRAME_LEN);
        
        // Set up pointers to frame parts
        fhead = (struct ethhdr*) frame;
        fdata = frame + ETH_HLEN;
        
        // Receive frame from network interface
        len = recvfrom(sfd, frame, ETH_FRAME_LEN, 0, NULL, NULL);
        if (len < 0) {
            perror("recvfrom");
            free(frame);
            break;
        }
        
        printf("Received frame [%d bytes] from %02x:%02x:%02x:%02x:%02x:%02x\n",
               (int)len,
               fhead->h_source[0], fhead->h_source[1], fhead->h_source[2],
               fhead->h_source[3], fhead->h_source[4], fhead->h_source[5]);
        
        // Modify Ethernet header for forwarding
        // Set destination MAC address to the target specified in command line
        memcpy(fhead->h_dest, &sall2.sll_addr, ETH_ALEN);
        
        // Set source MAC address to our interface's MAC address
        memcpy(fhead->h_source, &ifr.ifr_hwaddr.sa_data, ETH_ALEN);
        
        // Forward the frame to the specified destination
        ssize_t sent = sendto(cfd, frame, len, 0, (struct sockaddr*) &sall2, sizeof(struct sockaddr_ll));
        if (sent < 0) {
            perror("sendto");
        } else {
            printf("Forwarded frame [%d bytes] to %02x:%02x:%02x:%02x:%02x:%02x\n\n",
                   (int)sent,
                   fhead->h_dest[0], fhead->h_dest[1], fhead->h_dest[2],
                   fhead->h_dest[3], fhead->h_dest[4], fhead->h_dest[5]);
        }
        
        free(frame);
    }
    
    close(sfd);
    close(cfd);
    return EXIT_SUCCESS;
}