#!/usr/bin/env python3
"""
Monitor Server - Etap 3: Condition Variables
Centralny serwer zarządzający distributed monitor z obsługą warunków.

Architektura:
- Mutex: kontrola dostępu do sekcji krytycznej
- Condition Variables: kolejki procesów oczekujących na warunki
- Centralized Control: wszystkie decyzje podejmowane przez serwer

Protokół komunikacyjny:
- ENTER_MONITOR: żądanie wejścia do monitora
- EXIT_MONITOR: żądanie wyjścia z monitora  
- WAIT_CONDITION: oczekiwanie na warunek (zwalnia mutex)
- SIGNAL_CONDITION: sygnalizacja warunku (budzi jeden proces)
- BROADCAST_CONDITION: broadcast warunku (budzi wszystkie procesy)
"""

import zmq
import json
import time
import threading
from collections import defaultdict, deque
from dataclasses import dataclass
from typing import Dict, Set, Optional, Any
import logging

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('MonitorServer')

@dataclass
class ClientState:
    """Stan klienta w systemie"""
    client_id: str
    has_mutex: bool = False
    waiting_condition: Optional[str] = None
    last_activity: float = 0.0

class MonitorServer:
    """
    Centralny serwer monitora rozproszonego.
    
    Zarządza:
    - mutex_owner: kto aktualnie ma dostęp do sekcji krytycznej
    - mutex_queue: kolejka procesów oczekujących na mutex
    - condition_queues: kolejki procesów oczekujących na konkretne warunki
    - clients: stan wszystkich aktywnych klientów
    """
    
    def __init__(self, port: int = 5555):
        self.port = port
        
        # Mutex management
        self.mutex_owner: Optional[str] = None
        self.mutex_queue: deque = deque()  # Kolejka FIFO dla mutex
        
        # Condition variables management
        self.condition_queues: Dict[str, deque] = defaultdict(deque)
        
        # Client tracking
        self.clients: Dict[str, ClientState] = {}
        
        # Threading
        self.lock = threading.Lock()  # Ochrona stanu serwera
        self.running = False
        
        # ZMQ setup
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        
    def start(self):
        """Uruchomienie serwera"""
        self.socket.bind(f"tcp://*:{self.port}")
        self.running = True
        logger.info(f"Monitor Server uruchomiony na porcie {self.port}")
        
        try:
            while self.running:
                # Oczekiwanie na żądanie (timeout 1s dla graceful shutdown)
                if self.socket.poll(1000):
                    message = self.socket.recv_json()
                    response = self._handle_request(message)
                    self.socket.send_json(response)
                    
        except KeyboardInterrupt:
            logger.info("Otrzymano sygnał przerwania")
        finally:
            self.stop()
    
    def stop(self):
        """Zatrzymanie serwera"""
        self.running = False
        self.socket.close()
        self.context.term()
        logger.info("Monitor Server zatrzymany")
    
    def _handle_request(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """
        Obsługa żądań od klientów.
        Wszystkie operacje są atomowe dzięki self.lock.
        """
        with self.lock:
            try:
                action = message.get('action')
                client_id = message.get('client_id')
                
                # Aktualizacja czasu aktywności klienta
                if client_id:
                    if client_id not in self.clients:
                        self.clients[client_id] = ClientState(client_id)
                    self.clients[client_id].last_activity = time.time()
                
                # Routing żądań
                if action == 'ENTER_MONITOR':
                    return self._handle_enter_monitor(client_id)
                elif action == 'EXIT_MONITOR':
                    return self._handle_exit_monitor(client_id)
                elif action == 'WAIT_CONDITION':
                    condition = message.get('condition')
                    return self._handle_wait_condition(client_id, condition)
                elif action == 'SIGNAL_CONDITION':
                    condition = message.get('condition')
                    return self._handle_signal_condition(client_id, condition)
                elif action == 'BROADCAST_CONDITION':
                    condition = message.get('condition')
                    return self._handle_broadcast_condition(client_id, condition)
                elif action == 'GET_STATUS':
                    return self._handle_get_status()
                else:
                    return {'status': 'ERROR', 'message': f'Nieznana akcja: {action}'}
                    
            except Exception as e:
                logger.error(f"Błąd obsługi żądania: {e}")
                return {'status': 'ERROR', 'message': str(e)}
    
    def _handle_enter_monitor(self, client_id: str) -> Dict[str, Any]:
        """
        Obsługa wejścia do monitora.
        
        Logika:
        1. Jeśli mutex jest wolny -> przyznaj natychmiast
        2. Jeśli mutex zajęty -> dodaj do kolejki
        """
        logger.info(f"Klient {client_id} żąda wejścia do monitora")
        
        if self.mutex_owner is None:
            # Mutex wolny - przyznaj natychmiast
            self.mutex_owner = client_id
            self.clients[client_id].has_mutex = True
            logger.info(f"Mutex przyznany klientowi {client_id}")
            return {'status': 'GRANTED'}
        else:
            # Mutex zajęty - dodaj do kolejki
            if client_id not in self.mutex_queue:
                self.mutex_queue.append(client_id)
                logger.info(f"Klient {client_id} dodany do kolejki mutex (pozycja {len(self.mutex_queue)})")
            return {'status': 'QUEUED', 'position': len(self.mutex_queue)}
    
    def _handle_exit_monitor(self, client_id: str) -> Dict[str, Any]:
        """
        Obsługa wyjścia z monitora.
        
        Logika:
        1. Sprawdź czy klient rzeczywiście ma mutex
        2. Zwolnij mutex
        3. Przyznaj mutex następnemu w kolejce (jeśli istnieje)
        """
        logger.info(f"Klient {client_id} wychodzi z monitora")
        
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Nie masz mutex - nie możesz wyjść'}
        
        # Zwolnij mutex
        self.mutex_owner = None
        self.clients[client_id].has_mutex = False
        
        # Przyznaj mutex następnemu w kolejce
        next_client = self._grant_mutex_to_next()
        
        if next_client:
            logger.info(f"Mutex przekazany do klienta {next_client}")
            return {'status': 'RELEASED', 'next_owner': next_client}
        else:
            logger.info("Mutex zwolniony, brak oczekujących")
            return {'status': 'RELEASED'}
    
    def _handle_wait_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """
        Obsługa oczekiwania na warunek.
        
        Kluczowa semantyka monitora:
        1. Klient MUSI mieć mutex żeby wykonać wait
        2. Wait ZWALNIA mutex i dodaje klienta do kolejki warunku
        3. Mutex zostaje przyznany następnemu w kolejce
        """
        logger.info(f"Klient {client_id} czeka na warunek '{condition}'")
        
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Musisz mieć mutex żeby wykonać wait'}
        
        # Zwolnij mutex
        self.mutex_owner = None
        self.clients[client_id].has_mutex = False
        self.clients[client_id].waiting_condition = condition
        
        # Dodaj do kolejki warunku
        self.condition_queues[condition].append(client_id)
        
        # Przyznaj mutex następnemu w kolejce
        next_client = self._grant_mutex_to_next()
        
        logger.info(f"Klient {client_id} oczekuje na '{condition}', mutex przekazany do {next_client}")
        return {'status': 'WAITING', 'condition': condition}
    
    def _handle_signal_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """
        Obsługa sygnalizacji warunku (budzi JEDEN proces).
        
        Logika:
        1. Tylko właściciel mutex może sygnalizować
        2. Budzi pierwszy proces z kolejki warunku
        3. Obudzony proces trafia do kolejki mutex
        """
        logger.info(f"Klient {client_id} sygnalizuje warunek '{condition}'")
        
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Musisz mieć mutex żeby sygnalizować'}
        
        # Sprawdź czy ktoś oczekuje na ten warunek
        if condition not in self.condition_queues or not self.condition_queues[condition]:
            logger.info(f"Brak procesów oczekujących na warunek '{condition}'")
            return {'status': 'SIGNALED', 'woken_processes': 0}
        
        # Obudź pierwszy proces z kolejki
        woken_client = self.condition_queues[condition].popleft()
        self.clients[woken_client].waiting_condition = None
        
        # Dodaj obudzonego klienta do kolejki mutex
        self.mutex_queue.append(woken_client)
        
        logger.info(f"Klient {woken_client} obudzony z warunku '{condition}' i dodany do kolejki mutex")
        return {'status': 'SIGNALED', 'woken_processes': 1, 'woken_client': woken_client}
    
    def _handle_broadcast_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """
        Obsługa broadcast warunku (budzi WSZYSTKIE procesy).
        
        Logika:
        1. Tylko właściciel mutex może broadcastować
        2. Budzi wszystkie procesy z kolejki warunku
        3. Wszystkie obudzone procesy trafiają do kolejki mutex
        """
        logger.info(f"Klient {client_id} broadcastuje warunek '{condition}'")
        
        if self.mutex_owner != client_id:
            return {'status': 'ERROR', 'message': 'Musisz mieć mutex żeby broadcastować'}
        
        # Sprawdź czy ktoś oczekuje na ten warunek
        if condition not in self.condition_queues or not self.condition_queues[condition]:
            logger.info(f"Brak procesów oczekujących na warunek '{condition}'")
            return {'status': 'BROADCASTED', 'woken_processes': 0}
        
        # Obudź wszystkie procesy z kolejki warunku
        woken_clients = []
        while self.condition_queues[condition]:
            woken_client = self.condition_queues[condition].popleft()
            self.clients[woken_client].waiting_condition = None
            self.mutex_queue.append(woken_client)
            woken_clients.append(woken_client)
        
        logger.info(f"Broadcast '{condition}': obudzono {len(woken_clients)} procesów")
        return {'status': 'BROADCASTED', 'woken_processes': len(woken_clients), 'woken_clients': woken_clients}
    
    def _handle_get_status(self) -> Dict[str, Any]:
        """Zwraca aktualny stan serwera (do debugowania)"""
        return {
            'status': 'OK',
            'mutex_owner': self.mutex_owner,
            'mutex_queue': list(self.mutex_queue),
            'condition_queues': {k: list(v) for k, v in self.condition_queues.items()},
            'active_clients': len(self.clients)
        }
    
    def _grant_mutex_to_next(self) -> Optional[str]:
        """
        Przyznaje mutex następnemu klientowi w kolejce.
        Zwraca ID klienta lub None jeśli kolejka pusta.
        """
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.clients[next_client].has_mutex = True
            return next_client
        return None

def main():
    """Uruchomienie serwera"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Monitor Server - Etap 3')
    parser.add_argument('--port', type=int, default=5555, help='Port serwera')
    args = parser.parse_args()
    
    server = MonitorServer(args.port)
    
    try:
        server.start()
    except KeyboardInterrupt:
        print("\nZatrzymywanie serwera...")
        server.stop()

if __name__ == "__main__":
    main()