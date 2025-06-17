import zmq
import logging
from collections import deque, defaultdict

# Konfiguracja loggera
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

class MonitorServer:
    """
    Centralny serwer monitora, zarządzający muteksem i zmiennymi warunkowymi.
    """
    def __init__(self, port=5555):
        self.port = port
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        
        # Stan monitora
        self.mutex_owner = None
        self.mutex_queue = deque()
        # NOWOŚĆ: Słownik przechowujący kolejki dla zmiennych warunkowych
        # defaultdict ułatwia tworzenie nowej kolejki, gdy warunek pojawia się po raz pierwszy
        self.condition_queues = defaultdict(deque)
        
        self.logger = logging.getLogger("MonitorServer")

    def start(self):
        """Uruchamia główną pętlę serwera."""
        self.socket.bind(f"tcp://*:{self.port}")
        self.logger.info(f"Monitor Server uruchomiony na porcie {self.port}")
        
        try:
            while True:
                message = self.socket.recv_json()
                client_id = message['client_id']
                command = message['command']
                
                self.logger.info(f"Otrzymano polecenie '{command}' od klienta {client_id}")

                response = self._handle_request(client_id, command, message.get('payload'))
                
                # Odpowiedź jest wysyłana tylko jeśli nie jest None
                if response is not None:
                    self.socket.send_json(response)

        except KeyboardInterrupt:
            self.logger.info("Otrzymano sygnał przerwania, zamykanie serwera.")
        finally:
            self.stop()

    def _handle_request(self, client_id, command, payload):
        """Przetwarza żądania od klientów i zarządza stanem monitora."""
        
        if command == 'ENTER':
            return self._handle_enter(client_id)
        
        elif command == 'EXIT':
            return self._handle_exit(client_id)
        
        # NOWOŚĆ: Obsługa zmiennych warunkowych
        elif command == 'WAIT':
            # Sprawdzenie, czy klient jest właścicielem muteksu
            if self.mutex_owner != client_id:
                return {'status': 'ERROR', 'message': 'Klient musi posiadać muteks, aby użyć wait()'}
            return self._handle_wait(client_id, payload['condition'])
            
        elif command == 'SIGNAL':
            if self.mutex_owner != client_id:
                return {'status': 'ERROR', 'message': 'Klient musi posiadać muteks, aby użyć signal()'}
            return self._handle_signal(client_id, payload['condition'])

        elif command == 'BROADCAST':
            if self.mutex_owner != client_id:
                return {'status': 'ERROR', 'message': 'Klient musi posiadać muteks, aby użyć broadcast()'}
            return self._handle_broadcast(client_id, payload['condition'])

        else:
            return {'status': 'ERROR', 'message': 'Nieznane polecenie'}

    def _handle_enter(self, client_id):
        if self.mutex_owner is None:
            self.mutex_owner = client_id
            self.logger.info(f"Mutex przyznany klientowi {client_id}")
            return {'status': 'GRANTED'}
        else:
            if client_id not in self.mutex_queue:
                self.mutex_queue.append(client_id)
            pos = self.mutex_queue.index(client_id) + 1
            self.logger.info(f"Klient {client_id} dodany do kolejki muteksu (pozycja {pos})")
            return {'status': 'QUEUED', 'position': pos}

    def _handle_exit(self, client_id):
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Klient nie jest właścicielem muteksu'}
        
        self._release_mutex()
        return {'status': 'OK'}

    # NOWOŚĆ: Logika dla wait, signal, broadcast
    def _handle_wait(self, client_id, condition):
        """Umieszcza klienta w kolejce warunku i zwalnia muteks."""
        self.condition_queues[condition].append(client_id)
        self.logger.info(f"Klient {client_id} czeka na warunek '{condition}'")
        self._release_mutex()
        # Nie wysyłamy odpowiedzi - klient będzie czekał na "obudzenie" i ponowne przyznanie muteksu
        return None

    def _handle_signal(self, client_id, condition):
        """Przenosi jednego klienta z kolejki warunku do kolejki muteksu."""
        if self.condition_queues[condition]:
            signaled_client = self.condition_queues[condition].popleft()
            self.mutex_queue.appendleft(signaled_client) # Dajemy priorytet obudzonym
            self.logger.info(f"Klient {client_id} zasygnalizował warunek '{condition}'. Klient {signaled_client} przeniesiony do kolejki muteksu.")
        else:
            self.logger.info(f"Klient {client_id} zasygnalizował warunek '{condition}', ale nikt nie czekał.")
        return {'status': 'OK'}

    def _handle_broadcast(self, client_id, condition):
        """Przenosi wszystkich klientów z kolejki warunku do kolejki muteksu."""
        if self.condition_queues[condition]:
            num_waiters = len(self.condition_queues[condition])
            for _ in range(num_waiters):
                signaled_client = self.condition_queues[condition].popleft()
                self.mutex_queue.appendleft(signaled_client)
            self.logger.info(f"Klient {client_id} zasygnalizował broadcast dla warunku '{condition}'. {num_waiters} klientów przeniesionych do kolejki muteksu.")
        else:
            self.logger.info(f"Klient {client_id} zasygnalizował broadcast dla warunku '{condition}', ale nikt nie czekał.")
        return {'status': 'OK'}
        
    def _release_mutex(self):
        """Zwalnia muteks i przyznaje go następnemu oczekującemu."""
        self.logger.info(f"Mutex zwolniony przez {self.mutex_owner}")
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.logger.info(f"Mutex przekazany do klienta {next_client}")
            # Powiadamiamy nowego właściciela, że otrzymał dostęp
            self.socket.send_json({'status': 'GRANTED'})
        else:
            self.mutex_owner = None
            self.logger.info("Mutex jest wolny, brak oczekujących.")
    
    def stop(self):
        """Zatrzymuje serwer i zwalnia zasoby."""
        self.socket.close()
        self.context.term()
        self.logger.info("Monitor Server zatrzymany")


if __name__ == "__main__":
    server = MonitorServer()
    server.start()