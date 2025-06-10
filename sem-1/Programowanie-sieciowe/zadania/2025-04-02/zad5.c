#include <pcap.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <linux/if_ether.h>
#include <linux/ip.h>
#include <arpa/inet.h>
#include <unistd.h>

/* Protocol numbers from IANA */
#define ICMP_PROTOCOL_NUMBER 1
#define TCP_PROTOCOL_NUMBER 6
#define UDP_PROTOCOL_NUMBER 17

/* Ethernet protocol types */
#define ETHERTYPE_IP    0x0800    /* IP protocol */
#define ETHERTYPE_ARP   0x0806    /* Address resolution protocol */

/* Global variables */
char* errbuf;
pcap_t* handle;

/* Structure to count different protocol types */
struct prot_counter {
    int icmp_count;
    int arp_count;
    int ip_count;
    int tcp_count;
    int udp_count;
    int rest_count;
} protocols_counter = {0, 0, 0, 0, 0, 0};

/* Cleanup function called on exit */
void cleanup() {
    if (handle) {
        pcap_close(handle);
    }
    if (errbuf) {
        free(errbuf);
    }
}

/* Signal handler for SIGINT (Ctrl+C) */
void stop(int signo) {
    printf("\n=== Protocol Statistics ===\n");
    printf("ARP:\t\t%d\n", protocols_counter.arp_count);
    printf("IP:\t\t%d\n", protocols_counter.ip_count);
    printf("IP/ICMP:\t%d\n", protocols_counter.icmp_count);
    printf("IP/UDP:\t\t%d\n", protocols_counter.udp_count);
    printf("IP/TCP:\t\t%d\n", protocols_counter.tcp_count);
    printf("Other:\t\t%d\n", protocols_counter.rest_count);
    printf("===========================\n");
    printf("Total packets: %d\n", 
           protocols_counter.arp_count + protocols_counter.ip_count + protocols_counter.rest_count);
    exit(EXIT_SUCCESS);
}

/* Packet processing callback function */
void trap(u_char *user, const struct pcap_pkthdr *h, const u_char *bytes) {
    struct ethhdr *eth = (struct ethhdr *)bytes;
    
    // Check Ethernet protocol type
    if (ntohs(eth->h_proto) == ETHERTYPE_ARP) {
        protocols_counter.arp_count++;
        printf("ARP packet captured\n");
    }
    else if (ntohs(eth->h_proto) == ETHERTYPE_IP) {
        protocols_counter.ip_count++;
        
        // Parse IP header (starts after Ethernet header)
        struct iphdr *iph = (struct iphdr *)(bytes + sizeof(struct ethhdr));
        
        // Check IP protocol field to determine transport layer protocol
        switch(iph->protocol) {
            case ICMP_PROTOCOL_NUMBER:
                protocols_counter.icmp_count++;
                printf("IP/ICMP packet captured\n");
                break;
            case TCP_PROTOCOL_NUMBER:
                protocols_counter.tcp_count++;
                printf("IP/TCP packet captured\n");
                break;
            case UDP_PROTOCOL_NUMBER:
                protocols_counter.udp_count++;
                printf("IP/UDP packet captured\n");
                break;
            default:
                printf("IP/Other protocol (%d) packet captured\n", iph->protocol);
                break;
        }
    }
    else {
        protocols_counter.rest_count++;
        printf("Other protocol (EtherType: 0x%04x) packet captured\n", ntohs(eth->h_proto));
    }
}

int main(int argc, char** argv) {
    // Check arguments
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <interface_name>\n", argv[0]);
        fprintf(stderr, "Example: %s eth0\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    // Set up cleanup and signal handling
    atexit(cleanup);
    signal(SIGINT, stop);
    
    // Allocate error buffer
    errbuf = malloc(PCAP_ERRBUF_SIZE);
    if (!errbuf) {
        fprintf(stderr, "Failed to allocate error buffer\n");
        return EXIT_FAILURE;
    }
    
    printf("Starting packet capture on interface: %s\n", argv[1]);
    printf("Press Ctrl+C to stop and display statistics\n\n");
    
    // Create pcap handle
    handle = pcap_create(argv[1], errbuf);
    if (!handle) {
        fprintf(stderr, "Error creating pcap handle: %s\n", errbuf);
        return EXIT_FAILURE;
    }
    
    // Set promiscuous mode
    if (pcap_set_promisc(handle, 1) != 0) {
        fprintf(stderr, "Error setting promiscuous mode: %s\n", pcap_geterr(handle));
        return EXIT_FAILURE;
    }
    
    // Set snapshot length (maximum bytes per packet)
    if (pcap_set_snaplen(handle, 65535) != 0) {
        fprintf(stderr, "Error setting snaplen: %s\n", pcap_geterr(handle));
        return EXIT_FAILURE;
    }
    
    // Set timeout (in milliseconds)
    if (pcap_set_timeout(handle, 1000) != 0) {
        fprintf(stderr, "Error setting timeout: %s\n", pcap_geterr(handle));
        return EXIT_FAILURE;
    }
    
    // Activate the handle
    int activate_result = pcap_activate(handle);
    if (activate_result < 0) {
        fprintf(stderr, "Error activating pcap handle: %s\n", pcap_geterr(handle));
        return EXIT_FAILURE;
    }
    else if (activate_result > 0) {
        printf("Warning during activation: %s\n", pcap_geterr(handle));
    }
    
    // Start packet capture loop
    printf("Capture started. Listening for packets...\n\n");
    
    // pcap_loop with -1 means capture indefinitely until error or signal
    int loop_result = pcap_loop(handle, -1, trap, NULL);
    
    if (loop_result == -1) {
        fprintf(stderr, "Error in pcap_loop: %s\n", pcap_geterr(handle));
        return EXIT_FAILURE;
    }
    
    return EXIT_SUCCESS;
}