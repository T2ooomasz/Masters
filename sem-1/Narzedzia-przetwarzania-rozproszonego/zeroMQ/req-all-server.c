#include <zmq.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>

void test_rep_client(char *server_address) {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_REQ);
    
    zmq_connect(socket, server_address);
    
    char *message = "Hello from Jan Kowalski";
    char buffer[200];
    
    printf("Testing REP server at %s\n", server_address);
    zmq_send(socket, message, strlen(message), 0);
    zmq_recv(socket, buffer, 200, 0);
    printf("Response: %s\n\n", buffer);
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

void test_sub_client(char *server_address) {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_SUB);
    
    zmq_connect(socket, server_address);
    zmq_setsockopt(socket, ZMQ_SUBSCRIBE, "", 0);
    
    char buffer[50];
    
    printf("Testing PUB server at %s\n", server_address);
    printf("Receiving 3 messages:\n");
    
    for(int i = 0; i < 3; i++) {
        zmq_recv(socket, buffer, 50, 0);
        printf("Received: %s\n", buffer);
    }
    printf("\n");
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

void test_push_client(char *server_address) {
    void *context = zmq_ctx_new();
    void *socket = zmq_socket(context, ZMQ_PUSH);
    
    zmq_connect(socket, server_address);
    
    printf("Testing PULL server at %s\n", server_address);
    
    for(int i = 0; i < 3; i++) {
        char message[100];
        snprintf(message, sizeof(message), "Test message %d from Jan Kowalski", i+1);
        zmq_send(socket, message, strlen(message), 0);
        printf("Sent: %s\n", message);
        sleep(1);
    }
    printf("\n");
    
    zmq_close(socket);
    zmq_ctx_destroy(context);
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        printf("Usage: %s <type> <server_address>\n", argv[0]);
        printf("Types: rep, sub, push\n");
        printf("Example: %s rep tcp://lab-os-6:5610\n", argv[0]);
        return 1;
    }
    
    char *type = argv[1];
    char *address = argv[2];
    
    printf("Replace 'Jan Kowalski' with your actual name!\n\n");
    
    if (strcmp(type, "rep") == 0) {
        test_rep_client(address);
    } else if (strcmp(type, "sub") == 0) {
        test_sub_client(address);
    } else if (strcmp(type, "push") == 0) {
        test_push_client(address);
    } else {
        printf("Invalid type. Use: rep, sub, or push\n");
        return 1;
    }
    
    return 0;
}