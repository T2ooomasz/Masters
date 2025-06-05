#include <zmq.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main() {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_SUB);
    
    // Connect to PUB service on port 6005
    zmq_connect(socket, "tcp://unixlab.cs.put.poznan.pl:6005");
    
    // Subscribe to all messages (empty filter means receive all)
    zmq_setsockopt(socket, ZMQ_SUBSCRIBE, "", 0);
    
    char buffer[50];
    
    printf("Subscribing to service 2 (PUB socket on port 6005)...\n");
    printf("Waiting for 3 messages:\n\n");
    
    // Receive three messages
    for(int i = 0; i < 3; i++) {
        zmq_recv(socket, buffer, 50, 0);
        printf("Message %d: %s\n", i+1, buffer);
    }
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
    
    return 0;
}