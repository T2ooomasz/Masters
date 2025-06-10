#include <arpa/inet.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <unistd.h>

#define IPPROTO_CUSTOM 222

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <DESTINATION_IP>\n", argv[0]);
        return EXIT_FAILURE;
    }

    int sfd = socket(PF_INET, SOCK_RAW, IPPROTO_CUSTOM);
    if (sfd < 0) {
        perror("socket creation failed");
        return EXIT_FAILURE;
    }

    int sfd2 = socket(PF_INET, SOCK_RAW, IPPROTO_CUSTOM);
    if (sfd2 < 0) {
        perror("second socket creation failed");
        close(sfd);
        return EXIT_FAILURE;
    }

    struct sockaddr_in addr2 = {0};
    addr2.sin_family = AF_INET;
    addr2.sin_port = 0;
    if (inet_pton(AF_INET, argv[1], &addr2.sin_addr) <= 0) {
        fprintf(stderr, "Invalid destination address: %s\n", argv[1]);
        close(sfd);
        close(sfd2);
        return EXIT_FAILURE;
    }

    char buf[65536];
    struct sockaddr_in addr;
    socklen_t sl;

    while (1) {
        sl = sizeof(addr);
        ssize_t rc = recvfrom(sfd, buf, sizeof(buf), 0, (struct sockaddr *)&addr, &sl);
        if (rc < 0) {
            perror("recvfrom failed");
            break;
        }

        struct iphdr *ip = (struct iphdr *)buf;
        if (ip->protocol == IPPROTO_CUSTOM) {
            char *data = (char *)ip + (ip->ihl * 4);
            if (sendto(sfd2, data, rc - (ip->ihl * 4), 0, (struct sockaddr *)&addr2, sizeof(addr2)) < 0) {
                perror("sendto failed");
            }
        }
    }

    close(sfd);
    close(sfd2);
    return EXIT_SUCCESS;
}