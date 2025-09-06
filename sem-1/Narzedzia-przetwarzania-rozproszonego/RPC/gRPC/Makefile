CXX = g++
CXXFLAGS = -std=c++17 -Wall
LDFLAGS = `pkg-config --libs protobuf grpc++ grpc`

# Pliki zrodlowe
PROTO_SOURCES = licznik.pb.cc licznik.grpc.pb.cc
SERVER_SOURCES = licznik_server.cpp $(PROTO_SOURCES)
CLIENT_SOURCES = licznik_client.cpp $(PROTO_SOURCES)

# Cele kompilacji
all: server client

server: licznik_server
client: licznik_client

# Kompilacja

licznik_server: $(SERVER_SOURCES)
	$(CXX) $(CXXFLAGS) -o licznik_server $(SERVER_SOURCES) $(LDFLAGS)

licznik_client: $(CLIENT_SOURCES)
	$(CXX) $(CXXFLAGS) -o licznik_client $(CLIENT_SOURCES) $(LDFLAGS)

# Generowanie kodu protobuf
proto:
	protoc licznik.proto --grpc_out=. --plugin=protoc-gen-grpc=/usr/bin/grpc_cpp_plugin
	protoc licznik.proto --cpp_out=.

# Czyszczenie
clean:
	rm -f licznik_server licznik_client *.pb.cc *.pb.h

.PHONY: all server client proto clean