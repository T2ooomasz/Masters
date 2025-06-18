import zmq
import uuid
import logging
import time

# Można odkomentować, aby włączyć logowanie klienta podczas normalnego działania
# logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

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
        self._connect()
        request = {'client_id': self.client_id, 'command': command, 'payload': payload}
        self.socket.send_json(request)
        return self.socket.recv_json()

    def enter(self):
        if self._has_mutex:
            raise RuntimeError("Błąd: Próba ponownego wejścia do monitora.")
            
        self.logger.info("Próba wejścia do monitora...")
        
        while not self._has_mutex:
            response = self._send_request('ENTER')
            if response['status'] == 'GRANTED':
                self._has_mutex = True
                self.logger.info("Wszedłem do monitora.")
            elif response['status'] == 'QUEUED':
                self.logger.info(f"W kolejce po muteks (pozycja {response['position']})")
                time.sleep(0.1)
            else:
                raise Exception(f"Nie udało się wejść do monitora: {response.get('message')}")

    def exit(self):
        if not self._has_mutex:
            raise RuntimeError("Błąd: Próba wyjścia z monitora bez wejścia.")
            
        self.logger.info("Wychodzenie z monitora...")
        response = self._send_request('EXIT')
        if response['status'] == 'OK':
            self._has_mutex = False
            self.logger.info("Wyszedłem z monitora.")
        else:
            raise Exception(f"Błąd przy wychodzeniu z monitora: {response.get('message')}")

    def wait(self, condition_name):
        if not self._has_mutex:
            raise RuntimeError("Błąd: wait() musi być wywołane wewnątrz monitora.")

        self.logger.info(f"Czekam na warunek '{condition_name}'...")
        response = self._send_request('WAIT', {'condition': condition_name})

        if response['status'] != 'OK':
            raise Exception(f"Serwer odrzucił prośbę o wait: {response.get('message')}")
        
        self._has_mutex = False
        self.logger.info("Zwolniłem muteks, aby czekać. Próbuję wejść ponownie...")
        
        self.enter()

    def signal(self, condition_name):
        if not self._has_mutex:
            raise RuntimeError("Błąd: signal() musi być wywołane wewnątrz monitora.")
        self._send_request('SIGNAL', {'condition': condition_name})
        self.logger.info(f"Warunek '{condition_name}' zasygnalizowany.")

    def broadcast(self, condition_name):
        if not self._has_mutex:
            raise RuntimeError("Błąd: broadcast() musi być wywołane wewnątrz monitora.")
        self._send_request('BROADCAST', {'condition': condition_name})
        self.logger.info(f"Broadcast dla warunku '{condition_name}' wysłany.")

    def close(self):
        """POPRAWKA: Przywracamy zamykanie kontekstu."""
        if self._is_connected:
            self.socket.close()
            self.context.term() # Ta linia jest kluczowa do uniknięcia ResourceWarning
            self._is_connected = False

    def __enter__(self):
        self.enter()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self._has_mutex:
            self.exit()
        self.close()