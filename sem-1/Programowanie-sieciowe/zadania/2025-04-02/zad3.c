#include <arpa/inet.h>
#include <linux/if_arp.h>
#include <linux/if_ether.h>
#include <linux/if_packet.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>
#include <linux/filter.h>

#define ETH_P_CUSTOM 0x8888

int main(int argc, char** argv) {
    int sfd, i;
    ssize_t len;
    char* frame;
    char* fdata;
    struct ethhdr* fhead;
    struct ifreq ifr;
    struct sockaddr_ll sall, sender;

    // Check if interface name is provided
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <interface_name>\n", argv[0]);
        return EXIT_FAILURE;
    }

    // BPF filter code for UDP packets (port 53 - DNS)
    // This filter will accept UDP packets on port 53 for both IPv4 and IPv6
    struct sock_filter code[] = {
        // Load EtherType (offset 12-13 in Ethernet header)
        { 0x28, 0, 0, 0x0000000c },  // ldh [12]
        
        // Check if it's IPv6 (0x86dd)
        { 0x15, 0, 6, 0x000086dd },  // jeq #0x86dd, jt 3, jf 9
        
        // IPv6 path: Load next header (offset 20 in IPv6 header)
        { 0x30, 0, 0, 0x00000014 },  // ldb [20]
        
        // Check if it's UDP (17)
        { 0x15, 0, 15, 0x00000011 }, // jeq #17, jt 5, jf 20
        
        // Load destination port (offset 54 in IPv6+UDP)
        { 0x28, 0, 0, 0x00000036 },  // ldh [54]
        
        // Check if destination port is 53 (DNS)
        { 0x15, 12, 0, 0x00000035 }, // jeq #53, jt 18, jf 7
        
        // Load source port (offset 56 in IPv6+UDP)
        { 0x28, 0, 0, 0x00000038 },  // ldh [56]
        
        // Check if source port is 53 (DNS)
        { 0x15, 10, 11, 0x00000035 }, // jeq #53, jt 18, jf 19
        
        // IPv4 path: Check if it's IPv4 (0x0800)
        { 0x15, 0, 10, 0x00000800 }, // jeq #0x800, jt 10, jf 20
        
        // Load protocol field (offset 23 in IPv4 header)
        { 0x30, 0, 0, 0x00000017 },  // ldb [23]
        
        // Check if it's UDP (17)
        { 0x15, 0, 8, 0x00000011 },  // jeq #17, jt 12, jf 20
        
        // Load fragment info to check if it's fragmented
        { 0x28, 0, 0, 0x00000014 },  // ldh [20]
        
        // Check fragment flags
        { 0x45, 6, 0, 0x00001fff },  // jset #0x1fff, jt 20, jf 14
        
        // Load IHL (Internet Header Length) and calculate UDP header offset
        { 0xb1, 0, 0, 0x0000000e },  // ldxb 4*([14]&0xf)
        
        // Load destination port
        { 0x48, 0, 0, 0x0000000e },  // ldh [x + 14]
        
        // Check if destination port is 53 (DNS)
        { 0x15, 2, 0, 0x00000035 },  // jeq #53, jt 18, jf 17
        
        // Load source port
        { 0x48, 0, 0, 0x00000010 },  // ldh [x + 16]
        
        // Check if source port is 53 (DNS)
        { 0x15, 0, 1, 0x00000035 },  // jeq #53, jt 18, jf 19
        
        // Accept packet (return 262144 bytes)
        { 0x6, 0, 0, 0x00040000 },   // ret #262144
        
        // Reject packet (return 0 bytes)
        { 0x6, 0, 0, 0x00000000 }    // ret #0
    };

    // Create BPF program structure
    struct sock_fprog bpf = { 
        .len = (sizeof(code)/sizeof(code[0])), 
        .filter = code
    };

    // Create raw socket
    sfd = socket(PF_PACKET, SOCK_RAW, htons(ETH_P_CUSTOM));
    if (sfd < 0) {
        perror("socket");
        return EXIT_FAILURE;
    }

    // Attach BPF filter to socket
    if (setsockopt(sfd, SOL_SOCKET, SO_ATTACH_FILTER, &bpf, sizeof(bpf)) < 0) {
        perror("setsockopt SO_ATTACH_FILTER");
        close(sfd);
        return EXIT_FAILURE;
    }

    // Get interface information
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
    
    // Configure socket address
    sall.sll_family = AF_PACKET;
    sall.sll_protocol = htons(ETH_P_CUSTOM);
    sall.sll_ifindex = ifr.ifr_ifindex;
    sall.sll_hatype = ARPHRD_ETHER;
    sall.sll_pkttype = PACKET_HOST;
    sall.sll_halen = ETH_ALEN;
    
    socklen_t addr_len = sizeof(sender);
    
    // Bind socket to interface
    if (bind(sfd, (struct sockaddr*) &sall, sizeof(struct sockaddr_ll)) < 0) {
        perror("bind");
        close(sfd);
        return EXIT_FAILURE;
    }

    printf("Listening for filtered frames on interface %s...\n", argv[1]);
    printf("Filter: UDP packets on port 53 (DNS)\n\n");

    // Main loop - receive and process filtered frames
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
        
        // Receive frame (filtered by BPF)
        len = recvfrom(sfd, frame, ETH_FRAME_LEN, 0, (struct sockaddr*)&sender, &addr_len);
        if (len < 0) {
            perror("recvfrom");
            free(frame);
            break;
        }

        // Display frame information
        printf("[Sender] [%dB] %02x:%02x:%02x:%02x:%02x:%02x -> ", (int)len,
               fhead->h_source[0], fhead->h_source[1], fhead->h_source[2],
               fhead->h_source[3], fhead->h_source[4], fhead->h_source[5]);

        printf("[Receiver] %02x:%02x:%02x:%02x:%02x:%02x | [Data] ",
               fhead->h_dest[0], fhead->h_dest[1], fhead->h_dest[2],
               fhead->h_dest[3], fhead->h_dest[4], fhead->h_dest[5]);
        
        printf("%s\n", fdata);
        printf("[EtherType] %ix%02x\n", sender.sll_pkttype, fhead->h_proto);
        
        // Display raw frame data
        for (i = 0; i < len; i++) {
            printf("%02x ", (unsigned char) frame[i]);
            if ((i + 1) % 16 == 0)
                printf("\n");
        }
        printf("\n\n");
        
        free(frame);
    }
    
    close(sfd);
    return EXIT_SUCCESS;
}