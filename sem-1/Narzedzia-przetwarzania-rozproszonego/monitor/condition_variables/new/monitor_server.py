#!/usr/bin/env python3
"""
Distributed Monitor Server - Centralny koordynator
Zarządza mutex'em i zmiennymi warunków dla rozproszonego monitora
"""

import zmq
import json
import time
import threading
import logging
from collections import defaultdict, deque
from typing import Dict, Set, Optional, Any
import signal
import sys

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('MonitorServer')

class MonitorServer:
    """Centralny serwer zarządzający monitorem rozproszonym"""
    
    def __init__(self, port: int = 5555):
        self.port = port
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        
        # Stan monitora
        self.mutex_owner: Optional[str] = None
        self.mutex_queue: deque = deque()  # Kolejka oczekujących na mutex
        
        # Zmienne warunków - każda ma swoją kolejkę oczekujących
        self.condition_queues: Dict[str, deque] = defaultdict(deque)
        
        # Aktywne klienty w monitorze
        self.active_clients: Set[str] = set()
        
        # Klienty oczekujące na warunki
        self.waiting_clients: Dict[str, str] = {}  # client_id -> condition_name
        
        # Thread safety
        self.lock = threading.Lock()
        
        # Kontrola działania
        self.running = False
        
    def start(self):
        """Uruchomienie serwera"""
        try:
            self.socket.bind(f"tcp://*:{self.port}")
            self.running = True
            logger.info(f"Monitor Server uruchomiony na porcie {self.port}")
            
            # Obsługa sygnału SIGINT
            signal.signal(signal.SIGINT, self._signal_handler)
            
            self._message_loop()
            
        except Exception as e:
            logger.error(f"Błąd uruchamiania serwera: {e}")
            self.stop()
    
    def _signal_handler(self, signum, frame):
        """Obsługa sygnału przerwania"""
        logger.info("Otrzymano sygnał przerwania")
        self.stop()
    
    def stop(self):
        """Zatrzymanie serwera"""
        self.running = False
        if hasattr(self, 'socket'):
            self.socket.close()
        if hasattr(self, 'context'):
            self.context.term()
    
    def _message_loop(self):
        """Główna pętla obsługi wiadomości"""
        while self.running:
            try:
                # Czekaj na wiadomość z timeout
                if self.socket.poll(timeout=1000):  # 1 sekunda timeout
                    message = self.socket.recv_json()
                    response = self._handle_message(message)
                    self.socket.send_json(response)
                    
            except zmq.Again:
                continue  # Timeout - kontynuuj pętlę
            except zmq.ZMQError as e:
                if self.running:  # Tylko loguj błędy jeśli serwer nadal działa
                    logger.error(f"Błąd ZMQ: {e}")
                break
            except Exception as e:
                logger.error(f"Błąd w pętli wiadomości: {e}")
                if self.running:
                    self.socket.send_json({
                        'status': 'error',
                        'message': str(e)
                    })
    
    def _handle_message(self, message: Dict[str, Any]) -> Dict[str, Any]:
        """Obsługa pojedynczej wiadomości"""
        with self.lock:
            command = message.get('command')
            client_id = message.get('client_id')
            
            if command == 'ENTER_MONITOR':
                return self._handle_enter_monitor(client_id)
            elif command == 'EXIT_MONITOR':
                return self._handle_exit_monitor(client_id)
            elif command == 'WAIT_CONDITION':
                condition = message.get('condition')
                return self._handle_wait_condition(client_id, condition)
            elif command == 'SIGNAL_CONDITION':
                condition = message.get('condition')
                return self._handle_signal_condition(client_id, condition)
            elif command == 'BROADCAST_CONDITION':
                condition = message.get('condition')
                return self._handle_broadcast_condition(client_id, condition)
            elif command == 'GET_STATUS':
                return self._handle_get_status()
            else:
                return {
                    'status': 'error',
                    'message': f'Nieznana komenda: {command}'
                }
    
    def _handle_enter_monitor(self, client_id: str) -> Dict[str, Any]:
        """Obsługa żądania wejścia do monitora"""
        logger.info(f"Klient {client_id} żąda wejścia do monitora")
        
        # Sprawdź czy klient już jest w monitorze
        if client_id in self.active_clients:
            return {
                'status': 'error',
                'message': 'Klient już jest w monitorze'
            }
        
        # Jeśli mutex jest wolny, przyznaj go
        if self.mutex_owner is None:
            self.mutex_owner = client_id
            self.active_clients.add(client_id)
            logger.info(f"Mutex przyznany klientowi {client_id}")
            return {
                'status': 'granted',
                'message': 'Wejście do monitora przyznane'
            }
        else:
            # Dodaj do kolejki oczekujących
            self.mutex_queue.append(client_id)
            logger.info(f"Klient {client_id} dodany do kolejki oczekujących")
            return {
                'status': 'waiting',
                'message': 'Oczekiwanie na dostęp do monitora'
            }
    
    def _handle_exit_monitor(self, client_id: str) -> Dict[str, Any]:
        """Obsługa żądania wyjścia z monitora"""
        logger.info(f"Klient {client_id} wychodzi z monitora")
        
        # Sprawdź czy klient jest właścicielem mutex
        if self.mutex_owner != client_id:
            return {
                'status': 'error',
                'message': 'Klient nie jest właścicielem mutex'
            }
        
        # Usuń klienta z aktywnych
        self.active_clients.discard(client_id)
        self.mutex_owner = None
        
        # Usuń z kolejek warunków jeśli tam był
        if client_id in self.waiting_clients:
            condition = self.waiting_clients[client_id]
            if client_id in self.condition_queues[condition]:
                self.condition_queues[condition].remove(client_id)
            del self.waiting_clients[client_id]
        
        # Sprawdź czy są klienci oczekujący na mutex
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.active_clients.add(next_client)
            logger.info(f"Mutex przekazany klientowi {next_client}")
        else:
            logger.info("Mutex zwolniony, brak oczekujących")
        
        return {
            'status': 'success',
            'message': 'Wyjście z monitora zakończone'
        }
    
    def _handle_wait_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """Obsługa oczekiwania na warunek"""
        # Sprawdź czy klient jest w monitorze
        if client_id not in self.active_clients:
            return {
                'status': 'error',
                'message': 'Klient nie jest w monitorze'
            }
        
        # Dodaj klienta do kolejki warunku
        self.condition_queues[condition].append(client_id)
        self.waiting_clients[client_id] = condition
        
        # Zwolnij mutex - klient czeka na warunek
        self.active_clients.discard(client_id)
        self.mutex_owner = None
        
        # Sprawdź czy są klienci oczekujący na mutex
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.active_clients.add(next_client)
            logger.info(f"Mutex przekazany klientowi {next_client} (po wait)")
        
        logger.info(f"Klient {client_id} oczekuje na warunek '{condition}'")
        return {
            'status': 'waiting',
            'condition': condition,
            'message': f'Oczekiwanie na warunek {condition}'
        }
    
    def _handle_signal_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """Obsługa sygnalizacji warunku"""
        # Sprawdź czy klient jest w monitorze
        if client_id not in self.active_clients:
            return {
                'status': 'error',
                'message': 'Klient nie jest w monitorze'
            }
        
        # Sprawdź czy są klienci oczekujący na ten warunek
        if condition in self.condition_queues and self.condition_queues[condition]:
            awakened_client = self.condition_queues[condition].popleft()
            
            # Przenieś klienta z kolejki warunku do kolejki mutex
            if awakened_client in self.waiting_clients:
                del self.waiting_clients[awakened_client]
            
            self.mutex_queue.append(awakened_client)
            logger.info(f"Klient {awakened_client} obudzony z warunku '{condition}'")
            
            return {
                'status': 'success',
                'message': f'Sygnał wysłany do warunku {condition}',
                'awakened_client': awakened_client
            }
        else:
            return {
                'status': 'success',
                'message': f'Brak klientów oczekujących na warunek {condition}'
            }
    
    def _handle_broadcast_condition(self, client_id: str, condition: str) -> Dict[str, Any]:
        """Obsługa rozgłaszania warunku"""
        # Sprawdź czy klient jest w monitorze
        if client_id not in self.active_clients:
            return {
                'status': 'error',
                'message': 'Klient nie jest w monitorze'
            }
        
        awakened_count = 0
        awakened_clients = []
        
        # Obudź wszystkich klientów oczekujących na warunek
        if condition in self.condition_queues:
            while self.condition_queues[condition]:
                awakened_client = self.condition_queues[condition].popleft()
                
                # Przenieś do kolejki mutex
                if awakened_client in self.waiting_clients:
                    del self.waiting_clients[awakened_client]
                
                self.mutex_queue.append(awakened_client)
                awakened_clients.append(awakened_client)
                awakened_count += 1
        
        if awakened_count > 0:
            logger.info(f"Broadcast warunku '{condition}' obudził {awakened_count} klientów")
        
        return {
            'status': 'success',
            'message': f'Broadcast wysłany do warunku {condition}',
            'awakened_count': awakened_count,
            'awakened_clients': awakened_clients
        }
    
    def _handle_get_status(self) -> Dict[str, Any]:
        """Pobieranie stanu serwera"""
        return {
            'status': 'success',
            'server_status': {
                'mutex_owner': self.mutex_owner,
                'active_clients': list(self.active_clients),
                'mutex_queue': list(self.mutex_queue),
                'condition_queues': {
                    condition: list(queue) 
                    for condition, queue in self.condition_queues.items()
                },
                'waiting_clients': dict(self.waiting_clients)
            }
        }

def main():
    """Funkcja główna"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Distributed Monitor Server')
    parser.add_argument('--port', type=int, default=5555, 
                       help='Port serwera (domyślnie 5555)')
    parser.add_argument('--debug', action='store_true',
                       help='Włącz tryb debug')
    
    args = parser.parse_args()
    
    if args.debug:
        logging.getLogger().setLevel(logging.DEBUG)
    
    server = MonitorServer(port=args.port)
    
    try:
        server.start()
    except KeyboardInterrupt:
        logger.info("Serwer zatrzymany przez użytkownika")
    finally:
        server.stop()

if __name__ == "__main__":
    main()