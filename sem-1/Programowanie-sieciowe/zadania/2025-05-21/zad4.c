#include <netdb.h>      // For getaddrinfo, gai_strerror
#include <stdio.h>      // For perror, fprintf
#include <stdlib.h>     // For EXIT_FAILURE, EXIT_SUCCESS
#include <string.h>     // For memset
#include <sys/socket.h> // For socket, connect
#include <unistd.h>     // For read, write, close

/**
 * @brief Establishes a protocol-independent TCP connection to a host.
 *
 * This function attempts to connect to the specified host and service (port).
 * It automatically resolves addresses for both IPv4 and IPv6 and tries to
 * connect to the first one that succeeds.
 *
 * @param host The hostname or IP address (e.g., "localhost", "127.0.0.1", "::1").
 * @param service The service name or port number (e.g., "http", "80").
 * @return A valid socket file descriptor on success, or -1 on failure.
 */
int _connect(const char *host, const char *service) {
    int sfd = -1;
    int gai_error;
    struct addrinfo hints;
    struct addrinfo *res, *rp;

    // 1. Set up hints for getaddrinfo()
    memset(&hints, 0, sizeof(hints));
    hints.ai_family = AF_UNSPEC;     // Allow IPv4 or IPv6
    hints.ai_socktype = SOCK_STREAM; // We want a TCP socket

    // 2. Resolve the host and service names into a list of addresses
    gai_error = getaddrinfo(host, service, &hints, &res);
    if (gai_error != 0) {
        fprintf(stderr, "getaddrinfo: %s\n", gai_strerror(gai_error));
        return -1;
    }

    // 3. Loop through all returned addresses and try to connect
    // `rp` is our iterator pointer for the results linked list.
    for (rp = res; rp != NULL; rp = rp->ai_next) {
        // Try to create a socket with the current address family and type
        sfd = socket(rp->ai_family, rp->ai_socktype, rp->ai_protocol);
        if (sfd == -1) {
            perror("socket");
            continue; // On failure, try the next address
        }

        // Try to connect the socket to the address
        if (connect(sfd, rp->ai_addr, rp->ai_addrlen) == 0) {
            break; // Success!
        }

        // If connect failed, close the socket and try the next address
        perror("connect");
        close(sfd);
        sfd = -1;
    }

    // 4. Free the memory allocated by getaddrinfo
    freeaddrinfo(res);

    if (sfd == -1) {
        fprintf(stderr, "Could not connect to %s:%s\n", host, service);
    }

    return sfd; // Return the connected socket descriptor or -1 on failure
}

/**
 * @brief Main function for the client program.
 */
int main(int argc, char **argv) {
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <host> <service_or_port>\n", argv[0]);
        fprintf(stderr, "Example: %s localhost 1234\n", argv[0]);
        return EXIT_FAILURE;
    }

    // Connect to the server using our protocol-independent function
    int sfd = _connect(argv[1], argv[2]);
    if (sfd == -1) {
        return EXIT_FAILURE; // _connect() already printed an error
    }

    printf("Connected successfully. Waiting for data...\n");

    // Loop to read data from the socket and write it to standard output
    char buf[BUFSIZ]; // Use a standard buffer size
    ssize_t bytes_read;
    while ((bytes_read = read(sfd, buf, sizeof(buf))) > 0) {
        // Write the data we just read to standard output (fd 1)
        if (write(STDOUT_FILENO, buf, bytes_read) != bytes_read) {
            perror("write failed");
            close(sfd);
            return EXIT_FAILURE;
        }
    }

    // The loop exits when read() returns 0 (server closed connection) or -1 (error)
    if (bytes_read == -1) {
        perror("read failed");
    }

    printf("\nConnection closed.\n");
    close(sfd);
    return EXIT_SUCCESS;
}