#include <arpa/inet.h>
#include <netinet/ip_icmp.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

char *argv1;
int tx = 0, rx = 0;

void cleanup() {
    printf("\n--- %s ping statistics ---\n", argv1);
    int packet_loss = (tx > 0) ? ((tx - rx) * 100) / tx : 0;
    printf("%d packets transmitted, %d packets received, %d%% packet loss\n", tx, rx, packet_loss);
}

void stop(int signo) {
    (void)signo; // Avoid unused parameter warning
    exit(EXIT_SUCCESS);
}

void tdiff(struct timeval *t1, struct timeval *t2) {
    long sec_diff = t2->tv_sec - t1->tv_sec;
    long usec_diff = t2->tv_usec - t1->tv_usec;
    if (usec_diff < 0) {
        sec_diff--;
        usec_diff += 1000000;
    }
    t1->tv_sec = sec_diff;
    t1->tv_usec = usec_diff;
}

uint16_t chksum(uint16_t *addr, int len) {
    int nleft = len;
    uint32_t sum = 0;
    uint16_t *w = addr;
    uint16_t result;

    while (nleft > 1) {
        sum += *w++;
        nleft -= 2;
    }
    if (nleft == 1) {
        *(uint8_t *)&result = *(uint8_t *)w;
        sum += result;
    }
    sum = (sum >> 16) + (sum & 0xffff);
    sum += (sum >> 16);
    result = ~sum;
    return result;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <IP_ADDRESS>\n", argv[0]);
        return EXIT_FAILURE;
    }

    int sfd = socket(PF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (sfd < 0) {
        perror("socket creation failed");
        return EXIT_FAILURE;
    }

    struct sockaddr_in snd = {0};
    snd.sin_family = AF_INET;
    snd.sin_port = 0;
    if (inet_pton(AF_INET, argv[1], &snd.sin_addr) <= 0) {
        fprintf(stderr, "Invalid address: %s\n", argv[1]);
        close(sfd);
        return EXIT_FAILURE;
    }

    atexit(cleanup);
    signal(SIGINT, stop);
    argv1 = argv[1];

    struct icmphdr req = {0};
    req.type = ICMP_ECHO;
    req.code = 0;
    req.un.echo.id = htons(getpid() & 0xffff); // Unique ID based on PID

    char buf[2048];
    struct sockaddr_in rcv;
    socklen_t sl = sizeof(rcv);

    printf("PING %s\n", argv[1]);
    while (1) {
        tx++;
        req.un.echo.sequence = htons(tx);
        req.checksum = 0;
        req.checksum = chksum((uint16_t *)&req, sizeof(req));

        struct timeval out;
        gettimeofday(&out, NULL);
        if (sendto(sfd, &req, sizeof(req), 0, (struct sockaddr *)&snd, sizeof(snd)) < 0) {
            perror("sendto failed");
            break;
        }

        int rc = recvfrom(sfd, buf, sizeof(buf), 0, (struct sockaddr *)&rcv, &sl);
        if (rc < 0) {
            perror("recvfrom failed");
            break;
        }

        struct timeval in;
        gettimeofday(&in, NULL);
        if (rcv.sin_addr.s_addr == snd.sin_addr.s_addr) {
            struct iphdr *ip = (struct iphdr *)buf;
            struct icmphdr *rep = (struct icmphdr *)(buf + (ip->ihl * 4));
            if (rep->type == ICMP_ECHOREPLY && rep->un.echo.id == req.un.echo.id) {
                rx++;
                struct timeval diff;
                tdiff(&diff, &in); // Note: 'out' is modified, so use 'in' as base
                long rtt = diff.tv_sec * 1000000 + diff.tv_usec;
                printf("%d bytes from %s: icmp_seq=%d ttl=%d time=%.3f ms\n",
                       rc - (ip->ihl * 4), argv[1], ntohs(rep->un.echo.sequence),
                       ip->ttl, rtt / 1000.0);
            }
        }
        sleep(1);
    }

    close(sfd);
    return EXIT_SUCCESS;
}