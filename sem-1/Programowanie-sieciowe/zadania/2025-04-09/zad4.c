#include <pcap.h>
#include <libnet.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

// The ARP header structure, as defined in the task description.
// This allows us to interpret the raw bytes of a captured ARP packet.
struct arphdr {
    u_int16_t ftype;
    u_int16_t ptype;
    u_int8_t  flen;
    u_int8_t  plen;
    u_int16_t opcode;
    u_int8_t  sender_mac_addr[6];
    u_int8_t  sender_ip_addr[4];
    u_int8_t  target_mac_addr[6];
    u_int8_t  target_ip_addr[4];
};

// A custom struct to pass necessary data to the pcap callback function.
// This helps avoid the use of global variables.
struct callback_data {
    pcap_t* handle;
    clock_t start_time;
};

// The pcap callback function, executed for each captured packet.
void handle_response(u_char* args, const struct pcap_pkthdr* pkthdr, const u_char* packet) {
    // Cast the user-provided data back to our custom struct type.
    struct callback_data* data = (struct callback_data*)args;
    clock_t end_time = clock();

    // The packet starts with an Ethernet header. The ARP header comes after it.
    const struct arphdr* arp_header = (const struct arphdr*)(packet + sizeof(struct ethhdr));

    // Ensure the captured packet is an ARP reply (opcode 2).
    // ntohs() converts from network byte order to host byte order.
    if (ntohs(arp_header->opcode) == ARPOP_REPLY) {
        double elapsed_ms = (double)(end_time - data->start_time) * 1000.0 / CLOCKS_PER_SEC;

        printf("Unicast reply from %d.%d.%d.%d ",
               arp_header->sender_ip_addr[0],
               arp_header->sender_ip_addr[1],
               arp_header->sender_ip_addr[2],
               arp_header->sender_ip_addr[3]);

        printf("[%02x:%02x:%02x:%02x:%02x:%02x] ",
               arp_header->sender_mac_addr[0],
               arp_header->sender_mac_addr[1],
               arp_header->sender_mac_addr[2],
               arp_header->sender_mac_addr[3],
               arp_header->sender_mac_addr[4],
               arp_header->sender_mac_addr[5]);

        printf("%.3f ms\n", elapsed_ms);

        // Stop the capture loop now that we have our answer.
        pcap_breakloop(data->handle);
    }
}

int main(int argc, char** argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <interface> <target_ip>\n", argv[0]);
        return 1;
    }

    char* dev = argv[1];
    char* target_ip_str = argv[2];
    char errbuf[PCAP_ERRBUF_SIZE];

    libnet_t* ln;
    pcap_t* handle;

    u_int32_t target_ip_addr, src_ip_addr;
    struct libnet_ether_addr* src_mac_addr;

    // --- Initialization ---

    // Initialize libnet for packet injection
    ln = libnet_init(LIBNET_LINK, dev, errbuf);
    if (ln == NULL) {
        fprintf(stderr, "libnet_init() failed: %s\n", errbuf);
        return 1;
    }

    // Get the source IP and MAC address of our device
    src_ip_addr = libnet_get_ipaddr4(ln);
    src_mac_addr = libnet_get_hwaddr(ln);
    if (src_mac_addr == NULL) {
        fprintf(stderr, "Could not get source MAC address: %s\n", libnet_geterror(ln));
        libnet_destroy(ln);
        return 1;
    }

    // Convert the target IP from string to binary format
    target_ip_addr = libnet_name2addr4(ln, target_ip_str, LIBNET_DONT_RESOLVE);
    if (target_ip_addr == (u_int32_t)-1) {
        fprintf(stderr, "Invalid target IP address: %s\n", target_ip_str);
        libnet_destroy(ln);
        return 1;
    }

    // Initialize pcap for packet capture
    handle = pcap_create(dev, errbuf);
    if (handle == NULL) {
        fprintf(stderr, "pcap_create() failed: %s\n", errbuf);
        libnet_destroy(ln);
        return 1;
    }
    pcap_set_promisc(handle, 1);      // Set promiscuous mode
    pcap_set_timeout(handle, 1000);   // Set a 1-second timeout
    pcap_activate(handle);

    // --- Main Loop ---

    printf("ARPing %s from %s\n", target_ip_str, libnet_addr2name4(src_ip_addr, LIBNET_DONT_RESOLVE));

    while (1) {
        // Build the ARP request packet
        libnet_autobuild_arp(
            ARPOP_REQUEST,                  // Opcode: request
            src_mac_addr->ether_addr_octet, // Sender's MAC
            (u_int8_t*)&src_ip_addr,        // Sender's IP
            (u_int8_t*)"\x00\x00\x00\x00\x00\x00", // Target MAC (unknown)
            (u_int8_t*)&target_ip_addr,     // Target IP
            ln);

        // Build the Ethernet header (to broadcast MAC)
        libnet_autobuild_ethernet(
            (u_int8_t*)"\xff\xff\xff\xff\xff\xff", // Destination: broadcast
            ETHERTYPE_ARP,                  // Protocol type: ARP
            ln);

        // Prepare data for the callback
        struct callback_data data;
        data.handle = handle;
        data.start_time = clock();

        // Send the packet
        int bytes_written = libnet_write(ln);
        if (bytes_written == -1) {
            fprintf(stderr, "Warning: libnet_write() failed: %s\n", libnet_geterror(ln));
        }

        // Clear libnet's internal packet memory to prepare for the next one
        libnet_clear_packet(ln);

        // Start capturing, which will call handle_response upon packet arrival
        pcap_loop(handle, 1, handle_response, (u_char*)&data);

        // Pause for 1 second before the next ping
        sleep(1);
    }

    // --- Cleanup ---
    libnet_destroy(ln);
    pcap_close(handle);

    return 0;
}