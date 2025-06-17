import zmq
import logging
from collections import deque, defaultdict

# Konfiguracja loggera do debugowania
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')

class MonitorServer:
    """
    Centralny serwer monitora, zarządzający muteksem i zmiennymi warunkowymi.
    Finalna wersja odporna na zakleszczenia.
    """
    def __init__(self, port=5555):
        self.port = port
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        
        # --- Stan Monitora ---
        self.mutex_owner = None
        self.mutex_queue = deque()
        self.condition_queues = defaultdict(deque)

        # KLUCZOWA ZMIANA: Zbiór klientów, którzy są w stanie `wait` 
        # i nie mogą otrzymać muteksu, dopóki nie zostaną obudzeni.
        self.clients_in_wait_state = set()
        
        self.logger = logging.getLogger("MonitorServer")

    def start(self):
        """Uruchamia główną pętlę serwera."""
        self.socket.bind(f"tcp://*:{self.port}")
        self.logger.info(f"Monitor Server uruchomiony na porcie {self.port}")
        
        try:
            while True:
                message = self.socket.recv_json()
                response = self._handle_request(message)
                self.socket.send_json(response)
        except KeyboardInterrupt:
            self.logger.info("Otrzymano sygnał przerwania, zamykanie serwera.")
        finally:
            self.stop()

    def _handle_request(self, message):
        """Przetwarza żądania, uwzględniając stan oczekiwania klientów."""
        client_id = message['client_id']
        command = message['command']
        payload = message.get('payload')

        # --- Obsługa ENTER ---
        if command == 'ENTER':
            # Jeśli klient jest w stanie wait, nie może dostać muteksu. Musi czekać na sygnał.
            if client_id in self.clients_in_wait_state:
                return {'status': 'QUEUED', 'position': 'waiting_for_signal'}

            # Jeśli nikt nie ma muteksu, przyznaj go
            if self.mutex_owner is None:
                self.mutex_owner = client_id
                self.logger.info(f"Mutex przyznany klientowi {client_id}")
                return {'status': 'GRANTED'}
            # Jeśli to ten klient już ma muteks (np. ponawia prośbę), potwierdź
            elif self.mutex_owner == client_id:
                return {'status': 'GRANTED'}
            # W przeciwnym razie dodaj do kolejki
            else:
                if client_id not in self.mutex_queue:
                    self.mutex_queue.append(client_id)
                pos = self.mutex_queue.index(client_id) + 1
                self.logger.info(f"Klient {client_id} dodany do kolejki (pozycja {pos})")
                return {'status': 'QUEUED', 'position': pos}

        # Dla wszystkich innych komend, klient musi być właścicielem muteksu
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Operacja wymaga posiadania muteksu.'}

        # --- Inne operacje ---
        if command == 'EXIT':
            self._release_mutex()
            return {'status': 'OK'}
        
        elif command == 'WAIT':
            condition = payload['condition']
            self.condition_queues[condition].append(client_id)
            self.clients_in_wait_state.add(client_id) # Oznacz klienta jako czekającego
            self.logger.info(f"Klient {client_id} wchodzi w stan wait na warunek '{condition}'")
            self._release_mutex()
            return {'status': 'OK'}
            
        elif command == 'SIGNAL':
            condition = payload['condition']
            if self.condition_queues[condition]:
                signaled_client = self.condition_queues[condition].popleft()
                self.clients_in_wait_state.remove(signaled_client) # Klient już nie czeka
                self.mutex_queue.appendleft(signaled_client)
                self.logger.info(f"Klient {client_id} zasygnalizował '{condition}'. Obudzono {signaled_client}")
            return {'status': 'OK'}

        elif command == 'BROADCAST':
            condition = payload['condition']
            num_waiters = len(self.condition_queues[condition])
            if num_waiters > 0:
                for _ in range(num_waiters):
                    signaled_client = self.condition_queues[condition].popleft()
                    self.clients_in_wait_state.remove(signaled_client) # Usuń ze stanu wait
                    self.mutex_queue.appendleft(signaled_client)
                self.logger.info(f"Klient {client_id} zasygnalizował broadcast dla '{condition}'. Obudzono {num_waiters} klientów.")
            return {'status': 'OK'}

        return {'status': 'ERROR', 'message': 'Nieznane polecenie'}

    def _release_mutex(self):
        """Zwalnia muteks i logicznie przekazuje go następnemu w kolejce."""
        self.logger.info(f"Mutex zwolniony przez {self.mutex_owner}")
        if self.mutex_queue:
            self.mutex_owner = self.mutex_queue.popleft()
            self.logger.info(f"Mutex logicznie przekazany do klienta {self.mutex_owner}")
        else:
            self.mutex_owner = None
            self.logger.info("Mutex jest wolny, brak oczekujących.")
    
    def stop(self):
        """Zamyka gniazdo i kontekst."""
        self.socket.close()
        self.context.term()
        self.logger.info("Monitor Server zatrzymany")


if __name__ == "__main__":
    server = MonitorServer()
    server.start()