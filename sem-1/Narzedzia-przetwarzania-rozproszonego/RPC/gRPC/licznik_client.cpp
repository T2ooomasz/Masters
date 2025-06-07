/*
 * licznik_client.cpp - Interaktywny klient gRPC dla zdalnego licznika
 *
 * INSTRUKCJE KOMPILACJI I URUCHOMIENIA:
 * 
 * 1. Wygeneruj kod z definicji interfejsu:
 *    protoc licznik.proto --grpc_out=. --plugin=protoc-gen-grpc=`which grpc_cpp_plugin`
 *    protoc licznik.proto --cpp_out=.
 * 
 * 2. Skompiluj serwer:
 *    g++ licznik_server.cpp licznik.grpc.pb.cc licznik.pb.cc -std=c++17 `pkg-config --libs --static protobuf grpc++` -o licznik_server
 * 
 * 3. Skompiluj klienta:
 *    g++ licznik_client.cpp licznik.grpc.pb.cc licznik.pb.cc -std=c++17 `pkg-config --libs --static protobuf grpc++` -o licznik_client
 * 
 * 4. Uruchom serwer (w jednym terminalu):
 *    ./licznik_server
 * 
 * 5. Uruchom klienta (w drugim terminalu):
 *    ./licznik_client [host]
 *    
 *    Przykłady:
 *    ./licznik_client                 # localhost:50051
 *    ./licznik_client 192.168.1.100  # 192.168.1.100:50051
 */

#include <iostream>
#include <memory>
#include <string>
#include <sstream>
#include <grpcpp/grpcpp.h>
#include "licznik.grpc.pb.h"

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

class LicznikClient {
public:
    LicznikClient(std::shared_ptr<Channel> channel)
        : stub_(Licznik::NewStub(channel)) {}

    // Wywołanie metody zwieksz
    int Zwieksz(int wartosc) {
        Wartosc request;
        request.set_wartosc(wartosc);

        Wartosc reply;
        ClientContext context;

        Status status = stub_->zwieksz(&context, request, &reply);

        if (status.ok()) {
            return reply.wartosc();
        } else {
            std::cout << "Blad wywolania zwieksz: " << status.error_code() 
                      << " - " << status.error_message() << std::endl;
            return -1;
        }
    }

    // Wywołanie metody zmniejsz
    int Zmniejsz(int wartosc) {
        Wartosc request;
        request.set_wartosc(wartosc);

        Wartosc reply;
        ClientContext context;

        Status status = stub_->zmniejsz(&context, request, &reply);

        if (status.ok()) {
            return reply.wartosc();
        } else {
            std::cout << "Blad wywolania zmniejsz: " << status.error_code() 
                      << " - " << status.error_message() << std::endl;
            return -1;
        }
    }

private:
    std::unique_ptr<Licznik::Stub> stub_;
};

void wyswietlPomoc() {
    std::cout << "\nDostepne komendy:\n";
    std::cout << "+ <liczba>  - zwieksz licznik o podana wartosc\n";
    std::cout << "- <liczba>  - zmniejsz licznik o podana wartosc\n";
    std::cout << "help        - wyswietl te pomoc\n";
    std::cout << "quit        - zakoncz program\n";
    std::cout << "\nPrzyklady:\n";
    std::cout << "+ 5         - zwieksz o 5\n";
    std::cout << "- 3         - zmniejsz o 3\n\n";
}

int main(int argc, char** argv) {
    std::string server_address("localhost:50051");
    
    if (argc > 1) {
        server_address = std::string(argv[1]) + ":50051";
    }

    // Utworzenie kanału komunikacji
    LicznikClient licznik(grpc::CreateChannel(
        server_address, grpc::InsecureChannelCredentials()));

    std::cout << "=== Klient gRPC - Zdalny Licznik ===" << std::endl;
    std::cout << "Laczenie z serwerem: " << server_address << std::endl;
    std::cout << "Wpisz 'help' aby zobaczyc dostepne komendy" << std::endl;

    std::string linia;
    while (true) {
        std::cout << "> ";
        std::getline(std::cin, linia);
        
        // Pusta linia - kontynuuj
        if (linia.empty()) {
            continue;
        }
        
        // Sprawdzenie komend
        if (linia == "quit" || linia == "exit") {
            std::cout << "Zakonczenie programu..." << std::endl;
            break;
        } else if (linia == "help") {
            wyswietlPomoc();
        } else if (linia[0] == '+' || linia[0] == '-') {
            // Parsowanie operacji
            std::istringstream iss(linia);
            char operatorChar;
            int wartosc;
            
            if (iss >> operatorChar >> wartosc) {
                int wynik;
                if (operatorChar == '+') {
                    std::cout << "Wywoluje zwieksz(" << wartosc << ")" << std::endl;
                    wynik = licznik.Zwieksz(wartosc);
                    if (wynik != -1) {
                        std::cout << "Zwiekszono o " << wartosc << ". Nowa wartosc: " << wynik << std::endl;
                    }
                } else if (operatorChar == '-') {
                    std::cout << "Wywoluje zmniejsz(" << wartosc << ")" << std::endl;
                    wynik = licznik.Zmniejsz(wartosc);
                    if (wynik != -1) {
                        std::cout << "Zmniejszono o " << wartosc << ". Nowa wartosc: " << wynik << std::endl;
                    }
                } else {
                    std::cout << "Niepoprawny operator: " << operatorChar << ". Uzyj + lub -" << std::endl;
                }
            } else {
                std::cout << "Niepoprawny format. Uzyj: + <liczba> lub - <liczba>" << std::endl;
            }
        } else {
            std::cout << "Niepoprawna komenda. Wpisz 'help' aby zobaczyc dostepne komendy." << std::endl;
        }
    }

    return 0;
}