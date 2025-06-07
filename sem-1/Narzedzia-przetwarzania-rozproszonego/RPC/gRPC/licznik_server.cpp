/*
 * licznik_server.cpp - Implementacja serwera gRPC dla zdalnego licznika
 */

#include <iostream>
#include <memory>
#include <string>
#include <grpcpp/grpcpp.h>
#include "licznik.grpc.pb.h"

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;

// Implementacja serwisu Licznik
class LicznikServiceImpl final : public Licznik::Service {
private:
    int licznik_wartosc = 0;  // Stan licznika

public:
    // Implementacja metody zwieksz
    Status zwieksz(ServerContext* context, const Wartosc* request,
                   Wartosc* reply) override {
        std::cout << "Zwiększanie licznika o: " << request->wartosc() << std::endl;
        std::cout << "Wartość przed: " << licznik_wartosc << std::endl;
        
        licznik_wartosc += request->wartosc();
        reply->set_wartosc(licznik_wartosc);
        
        std::cout << "Wartość po: " << licznik_wartosc << std::endl;
        
        return Status::OK;
    }

    // Implementacja metody zmniejsz
    Status zmniejsz(ServerContext* context, const Wartosc* request,
                    Wartosc* reply) override {
        std::cout << "Zmniejszanie licznika o: " << request->wartosc() << std::endl;
        std::cout << "Wartość przed: " << licznik_wartosc << std::endl;
        
        licznik_wartosc -= request->wartosc();
        reply->set_wartosc(licznik_wartosc);
        
        std::cout << "Wartość po: " << licznik_wartosc << std::endl;
        
        return Status::OK;
    }
};

void RunServer() {
    std::string server_address("0.0.0.0:50051");
    LicznikServiceImpl service;

    ServerBuilder builder;
    // Nasłuchiwanie na określonym adresie bez uwierzytelniania
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    // Rejestracja serwisu
    builder.RegisterService(&service);
    
    // Zbudowanie i uruchomienie serwera
    std::unique_ptr<Server> server(builder.BuildAndStart());
    std::cout << "Serwer nasłuchuje na: " << server_address << std::endl;

    // Oczekiwanie na zakończenie serwera
    server->Wait();
}

int main(int argc, char** argv) {
    std::cout << "=== Serwer gRPC - Zdalny Licznik ===" << std::endl;
    RunServer();
    return 0;
}