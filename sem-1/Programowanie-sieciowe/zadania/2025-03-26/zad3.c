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
    int sfd;
    ssize_t len;
    char* frame;
    char* fdata;
    struct ethhdr* fhead;
    struct ifreq ifr;
    struct sockaddr_ll sall, sender;
    socklen_t addr_len = sizeof(sender);
    
    // Check if interface name is provided
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <interface_name>\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    // Create raw socket for custom EtherType
    sfd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_CUSTOM));
    if (sfd < 0) {
        perror("socket");
        return EXIT_FAILURE;
    }
    
    // Get interface index
    strncpy(ifr.ifr_name, argv[1], IFNAMSIZ - 1);
    ifr.ifr_name[IFNAMSIZ - 1] = '\0';
    if (ioctl(sfd, SIOCGIFINDEX, &ifr) < 0) {
        perror("ioctl SIOCGIFINDEX");
        close(sfd);
        return EXIT_FAILURE;
    }
    
    // Initialize sockaddr_ll structures
    memset(&sall, 0, sizeof(struct sockaddr_ll));
    memset(&sender, 0, sizeof(struct sockaddr_ll));
    
    // Configure socket address for binding
    sall.sll_family = AF_PACKET;
    sall.sll_protocol = htons(ETH_P_CUSTOM);
    sall.sll_ifindex = ifr.ifr_ifindex;
    sall.sll_hatype = ARPHRD_ETHER;
    sall.sll_pkttype = PACKET_HOST;
    sall.sll_halen = ETH_ALEN;
    
    // Bind socket to interface
    if (bind(sfd, (struct sockaddr*) &sall, sizeof(struct sockaddr_ll)) < 0) {
        perror("bind");
        close(sfd);
        return EXIT_FAILURE;
    }
    
    printf("Listening for frames on interface %s...\n\n", argv[1]);
    
    // Main loop - receive and process frames
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
        
        // Receive frame
        len = recvfrom(sfd, frame, ETH_FRAME_LEN, 0, (struct sockaddr*)&sender, &addr_len);
        if (len < 0) {
            perror("recvfrom");
            free(frame);
            break;
        }
        
        // Display frame information
        printf("=== Received Frame ===\n");
        printf("Frame size: %d bytes\n", (int)len);
        
        // Display MAC addresses
        printf("Source MAC: %02x:%02x:%02x:%02x:%02x:%02x\n",
               fhead->h_source[0], fhead->h_source[1], fhead->h_source[2],
               fhead->h_source[3], fhead->h_source[4], fhead->h_source[5]);
        
        printf("Destination MAC: %02x:%02x:%02x:%02x:%02x:%02x\n",
               fhead->h_dest[0], fhead->h_dest[1], fhead->h_dest[2],
               fhead->h_dest[3], fhead->h_dest[4], fhead->h_dest[5]);
        
        // Display packet type (from sockaddr_ll structure)
        printf("Packet type (sll_pkttype): %d\n", sender.sll_pkttype);
        
        // Display EtherType
        printf("EtherType: 0x%04x\n", ntohs(fhead->h_proto));
        
        // Display data (if any exists beyond Ethernet header)
        if (len > ETH_HLEN) {
            printf("Data: %s\n", fdata);
        }
        
        // Display raw frame data in hexadecimal
        printf("Raw frame data:\n");
        for (int i = 0; i < len; i++) {
            printf("%02x ", (unsigned char) frame[i]);
            if ((i + 1) % 16 == 0)
                printf("\n");
        }
        if (len % 16 != 0)
            printf("\n");
        
        printf("\n");
        
        free(frame);
    }
    
    close(sfd);
    return EXIT_SUCCESS;
}