import zmq
import uuid
import logging
import time

# Konfiguracja loggera
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

class DistributedMonitor:
    """
    Biblioteka kliencka zapewniająca transparentny dostęp do rozproszonego monitora.
    """
    def __init__(self, server_address):
        self.server_address = server_address
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REQ)
        self.client_id = f"client_{uuid.uuid4().hex[:8]}"
        self.logger = logging.getLogger(f"MonitorClient-{self.client_id}")
        self._is_connected = False
        self._has_mutex = False

    def _connect(self):
        if not self._is_connected:
            self.socket.connect(self.server_address)
            self._is_connected = True
            self.logger.info(f"Połączono z {self.server_address}")

    def _send_request(self, command, payload=None):
        """Wysyła żądanie do serwera i zwraca jego odpowiedź."""
        self._connect()
        request = {'client_id': self.client_id, 'command': command}
        if payload:
            request['payload'] = payload
        
        self.socket.send_json(request)
        return self.socket.recv_json()

    def enter(self):
        """Wejście do monitora (uzyskanie muteksu)."""
        if self._has_mutex:
            raise RuntimeError("Błąd: Próba ponownego wejścia do monitora bez jego opuszczenia.")
            
        self.logger.info("Próba wejścia do monitora...")
        
        # Pierwsze żądanie
        response = self._send_request('ENTER')
        
        # Pętla oczekiwania, jeśli serwer nas zakolejkował
        while response['status'] == 'QUEUED':
            self.logger.info(f"W kolejce po muteks (pozycja {response['position']})")
            time.sleep(1) # Czekamy chwilę przed ponownym zapytaniem o status
            response = self._send_request('ENTER')

        if response['status'] == 'GRANTED':
            self._has_mutex = True
            self.logger.info("Wszedłem do monitora.")
        else:
            raise Exception(f"Nie udało się wejść do monitora: {response.get('message')}")

    def exit(self):
        """Wyjście z monitora (zwolnienie muteksu)."""
        if not self._has_mutex:
            raise RuntimeError("Błąd: Próba wyjścia z monitora bez wcześniejszego wejścia.")
            
        self.logger.info("Wychodzenie z monitora...")
        response = self._send_request('EXIT')
        if response['status'] == 'OK':
            self._has_mutex = False
            self.logger.info("Wyszedłem z monitora.")
        else:
            # W przypadku błędu, stan klienta może być niespójny z serwerem.
            # W realnym systemie wymagałoby to zaawansowanej obsługi.
            raise Exception(f"Błąd przy wychodzeniu z monitora: {response.get('message')}")

    # NOWOŚĆ: Metody do obsługi zmiennych warunkowych
    def wait(self, condition_name):
        """Oczekiwanie na warunek."""
        if not self._has_mutex:
            raise RuntimeError("Błąd: wait() musi być wywołane wewnątrz monitora.")

        self.logger.info(f"Czekam na warunek '{condition_name}'...")
        self._connect()
        
        # Wysyłamy żądanie WAIT i zwalniamy muteks po stronie serwera
        request = {'client_id': self.client_id, 'command': 'WAIT', 'payload': {'condition': condition_name}}
        self.socket.send_json(request)
        self._has_mutex = False # Logicznie zwalniamy muteks
        
        # Czekamy na odpowiedź GRANTED od serwera, która oznacza, że zostaliśmy obudzeni
        # i ponownie otrzymaliśmy muteks
        response = self.socket.recv_json()

        if response['status'] == 'GRANTED':
            self._has_mutex = True
            self.logger.info(f"Obudzony i ponownie wszedłem do monitora po oczekiwaniu na '{condition_name}'.")
        else:
            raise Exception(f"Błąd podczas oczekiwania na warunek: {response.get('message')}")

    def signal(self, condition_name):
        """Sygnalizacja warunku."""
        if not self._has_mutex:
            raise RuntimeError("Błąd: signal() musi być wywołane wewnątrz monitora.")
        
        self.logger.info(f"Sygnalizuję warunek '{condition_name}'...")
        response = self._send_request('SIGNAL', {'condition': condition_name})

        if response['status'] != 'OK':
            raise Exception(f"Błąd podczas sygnalizacji warunku: {response.get('message')}")
        self.logger.info(f"Warunek '{condition_name}' zasygnalizowany.")

    def broadcast(self, condition_name):
        """Sygnalizacja warunku do wszystkich oczekujących."""
        if not self._has_mutex:
            raise RuntimeError("Błąd: broadcast() musi być wywołane wewnątrz monitora.")
            
        self.logger.info(f"Sygnalizuję broadcast dla warunku '{condition_name}'...")
        response = self._send_request('BROADCAST', {'condition': condition_name})

        if response['status'] != 'OK':
            raise Exception(f"Błąd podczas broadcast: {response.get('message')}")
        self.logger.info(f"Broadcast dla warunku '{condition_name}' wysłany.")

    def close(self):
        """Zamyka połączenie z serwerem."""
        if self._is_connected:
            self.socket.close()
            self.context.term()
            self._is_connected = False
            self.logger.info("Połączenie zamknięte.")

    # Użycie jako context manager (with DistributedMonitor(...) as monitor:)
    def __enter__(self):
        self.enter()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self._has_mutex:
            self.exit()
        self.close()