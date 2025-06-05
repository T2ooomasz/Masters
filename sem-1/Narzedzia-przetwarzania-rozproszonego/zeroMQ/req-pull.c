#include <zmq.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

int main() {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_PUSH);
    
    // Connect to PULL service on port 6001
    zmq_connect(socket, "tcp://unixlab.cs.put.poznan.pl:6001");
    
    // Your name and surname - replace with your actual name
    char *my_name = "Jan Kowalski"; // Replace with your actual name
    
    printf("Sending 3 messages to service 3 (PULL socket on port 6001)...\n\n");
    
    // Send three messages with your name
    for(int i = 0; i < 3; i++) {
        char message[100];
        snprintf(message, sizeof(message), "Message %d from %s", i+1, my_name);
        
        zmq_send(socket, message, strlen(message), 0);
        printf("Sent: %s\n", message);
        sleep(1); // Small delay between sends
    }
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
    
    return 0;
}