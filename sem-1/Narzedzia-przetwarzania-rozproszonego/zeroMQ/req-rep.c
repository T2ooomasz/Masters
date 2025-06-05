#include <zmq.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

int main() {
    void *context = zmq_ctx_new();
    void *socket1, *socket2;
    
    // Connect to port 5555
    socket1 = zmq_socket(context, ZMQ_REQ);
    zmq_connect(socket1, "tcp://unixlab.cs.put.poznan.pl:5555");
    
    // Connect to port 5556
    socket2 = zmq_socket(context, ZMQ_REQ);
    zmq_connect(socket2, "tcp://unixlab.cs.put.poznan.pl:5556");
    
    // Your student ID number - replace with your actual album number
    int album_number = 123456; // Replace with your actual student ID
    
    char buffer[100];
    
    printf("Sending requests to service 1...\n\n");
    
    // Three requests to port 5555
    for(int i = 0; i < 3; i++) {
        printf("Request %d to port 5555:\n", i+1);
        zmq_send(socket1, &album_number, sizeof(int), 0);
        zmq_recv(socket1, buffer, 100, 0);
        printf("Response: %s\n\n", buffer);
        sleep(1); // Small delay between requests
    }
    
    // Three requests to port 5556
    for(int i = 0; i < 3; i++) {
        printf("Request %d to port 5556:\n", i+4);
        zmq_send(socket2, &album_number, sizeof(int), 0);
        zmq_recv(socket2, buffer, 100, 0);
        printf("Response: %s\n\n", buffer);
        sleep(1); // Small delay between requests
    }
    
    zmq_close(socket1);
    zmq_close(socket2);
    zmq_ctx_destroy(context);
    
    return 0;
}