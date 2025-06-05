#include <zmq.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

void rep_server() {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_REP);
    
    // Bind to a port for REP service
    zmq_bind(socket, "tcp://*:5610");
    
    printf("REP server started on port 5610\n");
    
    char buffer[100];
    char response[200];
    
    while(1) {
        // Receive request
        zmq_recv(socket, buffer, 100, 0);
        printf("REP received: %s\n", buffer);
        
        // Create response with your name appended
        snprintf(response, sizeof(response), "%s - Response from Jan Kowalski", buffer);
        
        // Send response
        zmq_send(socket, response, strlen(response), 0);
    }
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

void pub_server() {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_PUB);
    
    // Bind to a port for PUB service
    zmq_bind(socket, "tcp://*:6010");
    
    printf("PUB server started on port 6010\n");
    
    int counter = 0;
    char message[50];
    
    while(1) {
        // Publish messages with your name
        snprintf(message, sizeof(message), "Message %d from Jan Kowalski", counter++);
        zmq_send(socket, message, strlen(message), 0);
        printf("PUB sent: %s\n", message);
        sleep(2); // Send every 2 seconds
    }
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

void pull_server() {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_PULL);
    
    // Bind to a port for PULL service
    zmq_bind(socket, "tcp://*:6015");
    
    printf("PULL server started on port 6015\n");
    
    char buffer[100];
    
    while(1) {
        // Receive messages
        zmq_recv(socket, buffer, 100, 0);
        printf("PULL received: %s\n", buffer);
    }
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

int main() {
    printf("Starting all three servers...\n");
    printf("Replace 'Jan Kowalski' with your actual name in the code!\n\n");
    
    pid_t pid1, pid2, pid3;
    
    // Fork for REP server
    pid1 = fork();
    if (pid1 == 0) {
        rep_server();
        exit(0);
    }
    
    // Fork for PUB server
    pid2 = fork();
    if (pid2 == 0) {
        pub_server();
        exit(0);
    }
    
    // Fork for PULL server
    pid3 = fork();
    if (pid3 == 0) {
        pull_server();
        exit(0);
    }
    
    // Parent process waits
    printf("All servers started. Press Ctrl+C to stop.\n");
    printf("Ports: REP=5610, PUB=6010, PULL=6015\n");
    
    // Wait for all children
    waitpid(pid1, NULL, 0);
    waitpid(pid2, NULL, 0);
    waitpid(pid3, NULL, 0);
    
    return 0;
}