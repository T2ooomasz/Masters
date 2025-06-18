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
import threading
import time
import logging
import signal
import sys
from collections import deque
from typing import Dict, Set, Optional

class MonitorServer:
    def __init__(self, port=5555):
        self.port = port
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        
        # Stan monitora
        self.mutex_owner = None  # ID klienta posiadającego mutex
        self.mutex_queue = deque()  # Kolejka oczekujących na mutex
        
        # Condition variables
        self.condition_queues = {}  # condition_name -> deque(client_ids)
        
        # Klienci w monitorze (dla wait/signal)
        self.clients_in_monitor = set()
        
        # Threading
        self.running = False
        self.server_thread = None
        
        # Logging
        self.logger = logging.getLogger('MonitorServer')
        
        # Graceful shutdown
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)
    
    def _signal_handler(self, signum, frame):
        self.logger.info("Otrzymano sygnał przerwania")
        self.stop()
    
    def start(self):
        """Uruchomienie serwera w osobnym wątku"""
        if self.running:
            return
        
        try:
            self.socket.bind(f"tcp://*:{self.port}")
            self.running = True
            self.logger.info(f"Monitor Server uruchomiony na porcie {self.port}")
            
            self.server_thread = threading.Thread(target=self._server_loop)
            self.server_thread.daemon = True
            self.server_thread.start()
            
        except Exception as e:
            self.logger.error(f"Błąd uruchamiania serwera: {e}")
            raise
    
    def _server_loop(self):
        """Główna pętla serwera"""
        while self.running:
            try:
                # Sprawdzenie czy są wiadomości (z timeout)
                if self.socket.poll(100, zmq.POLLIN):
                    message = self.socket.recv_json(zmq.NOBLOCK)
                    response = self._handle_message(message)
                    self.socket.send_json(response)
                    
            except zmq.Again:
                # Timeout - kontynuuj
                continue
            except zmq.ZMQError as e:
                if e.errno == zmq.ETERM:
                    break
                self.logger.error(f"Błąd ZMQ: {e}")
            except Exception as e:
                self.logger.error(f"Błąd w pętli serwera: {e}")
                # Wyślij błąd do klienta
                try:
                    self.socket.send_json({
                        'type': 'ERROR',
                        'message': str(e)
                    })
                except:
                    pass
    
    def _handle_message(self, message):
        """Obsługa wiadomości od klienta"""
        msg_type = message.get('type')
        client_id = message.get('client_id')
        
        self.logger.debug(f"Otrzymano wiadomość: {message}")
        
        if msg_type == 'ENTER_MONITOR':
            return self._handle_enter_monitor(client_id)
        elif msg_type == 'EXIT_MONITOR':
            return self._handle_exit_monitor(client_id)
        elif msg_type == 'WAIT_CONDITION':
            condition = message.get('condition')
            return self._handle_wait_condition(client_id, condition)
        elif msg_type == 'SIGNAL_CONDITION':
            condition = message.get('condition')
            return self._handle_signal_condition(client_id, condition)
        elif msg_type == 'BROADCAST_CONDITION':
            condition = message.get('condition')
            return self._handle_broadcast_condition(client_id, condition)
        elif msg_type == 'STATUS':
            return self._handle_status()
        else:
            return {'type': 'ERROR', 'message': f'Nieznany typ wiadomości: {msg_type}'}
    
    def _handle_enter_monitor(self, client_id):
        """Obsługa żądania wejścia do monitora"""
        self.logger.info(f"Klient {client_id} żąda wejścia do monitora")
        
        # Sprawdź czy klient już jest właścicielem mutex
        if self.mutex_owner == client_id:
            return {'type': 'ERROR', 'message': 'Klient już posiada mutex'}
        
        # Sprawdź czy klient już jest w kolejce
        if client_id in self.mutex_queue:
            position = list(self.mutex_queue).index(client_id) + 1
            return {
                'type': 'QUEUE_POSITION',
                'position': position,
                'message': f'Klient już w kolejce na pozycji {position}'
            }
        
        # Jeśli mutex jest wolny
        if self.mutex_owner is None:
            self.mutex_owner = client_id
            self.clients_in_monitor.add(client_id)
            self.logger.info(f"Mutex przyznany klientowi {client_id}")
            return {'type': 'MUTEX_GRANTED'}
        
        # Dodaj do kolejki
        self.mutex_queue.append(client_id)
        position = len(self.mutex_queue)
        self.logger.info(f"Klient {client_id} dodany do kolejki mutex (pozycja {position})")
        
        return {
            'type': 'QUEUE_POSITION',
            'position': position,
            'message': f'Dodano do kolejki na pozycji {position}'
        }
    
    def _handle_exit_monitor(self, client_id):
        """Obsługa wyjścia z monitora"""
        self.logger.info(f"Klient {client_id} wychodzi z monitora")
        
        # Sprawdź czy klient posiada mutex
        if self.mutex_owner != client_id:
            return {'type': 'ERROR', 'message': 'Klient nie posiada mutex'}
        
        # Zwolnij mutex
        self.mutex_owner = None
        self.clients_in_monitor.discard(client_id)
        
        # Usuń klienta ze wszystkich kolejek warunków
        for condition_queue in self.condition_queues.values():
            if client_id in condition_queue:
                condition_queue.remove(client_id)
        
        # Przekaż mutex następnemu w kolejce
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.clients_in_monitor.add(next_client)
            self.logger.info(f"Mutex przekazany do klienta {next_client}")
        else:
            self.logger.info("Mutex zwolniony, brak oczekujących")
        
        return {'type': 'EXIT_CONFIRMED'}
    
    def _handle_wait_condition(self, client_id, condition):
        """Obsługa oczekiwania na warunek"""
        # Sprawdź czy klient posiada mutex
        if self.mutex_owner != client_id:
            return {'type': 'ERROR', 'message': 'Klient musi posiadać mutex aby czekać'}
        
        # Dodaj do kolejki warunku
        if condition not in self.condition_queues:
            self.condition_queues[condition] = deque()
        
        self.condition_queues[condition].append(client_id)
        
        # Zwolnij mutex i przekaż następnemu
        self.mutex_owner = None
        self.clients_in_monitor.discard(client_id)
        
        if self.mutex_queue:
            next_client = self.mutex_queue.popleft()
            self.mutex_owner = next_client
            self.clients_in_monitor.add(next_client)
            self.logger.info(f"Mutex przekazany do klienta {next_client} (wait)")
        
        self.logger.info(f"Klient {client_id} czeka na warunek '{condition}'")
        return {'type': 'WAIT_CONFIRMED'}
    
    def _handle_signal_condition(self, client_id, condition):
        """Obsługa sygnalizacji warunku"""
        # Sprawdź czy klient posiada mutex
        if self.mutex_owner != client_id:
            return {'type': 'ERROR', 'message': 'Klient musi posiadać mutex aby sygnalizować'}
        
        # Sprawdź czy są oczekujący na ten warunek
        if condition not in self.condition_queues or not self.condition_queues[condition]:
            self.logger.info(f"Signal '{condition}' - brak oczekujących")
            return {'type': 'SIGNAL_CONFIRMED', 'woken': 0}
        
        # Obudź jednego oczekującego
        woken_client = self.condition_queues[condition].popleft()
        
        # Dodaj obudzonego klienta na początek kolejki mutex
        self.mutex_queue.appendleft(woken_client)
        
        self.logger.info(f"Signal '{condition}' - obudzono klienta {woken_client}")
        return {'type': 'SIGNAL_CONFIRMED', 'woken': 1}
    
    def _handle_broadcast_condition(self, client_id, condition):
        """Obsługa broadcast warunku"""
        # Sprawdź czy klient posiada mutex
        if self.mutex_owner != client_id:
            return {'type': 'ERROR', 'message': 'Klient musi posiadać mutex aby broadcastować'}
        
        # Sprawdź czy są oczekujący na ten warunek
        if condition not in self.condition_queues or not self.condition_queues[condition]:
            self.logger.info(f"Broadcast '{condition}' - brak oczekujących")
            return {'type': 'BROADCAST_CONFIRMED', 'woken': 0}
        
        # Obudź wszystkich oczekujących
        woken_clients = list(self.condition_queues[condition])
        self.condition_queues[condition].clear()
        
        # Dodaj obudzonych klientów do kolejki mutex (w odwrotnej kolejności)
        for woken_client in reversed(woken_clients):
            self.mutex_queue.appendleft(woken_client)
        
        woken_count = len(woken_clients)
        self.logger.info(f"Broadcast '{condition}' - obudzono {woken_count} klientów")
        return {'type': 'BROADCAST_CONFIRMED', 'woken': woken_count}
    
    def _handle_status(self):
        """Obsługa żądania statusu"""
        return {
            'type': 'STATUS',
            'mutex_owner': self.mutex_owner,
            'mutex_queue': list(self.mutex_queue),
            'clients_in_monitor': list(self.clients_in_monitor),
            'condition_queues': {
                condition: list(queue) 
                for condition, queue in self.condition_queues.items()
            }
        }
    
    def stop(self):
        """Zatrzymanie serwera"""
        if not self.running:
            return
        
        self.running = False
        
        # Zamknij socket
        try:
            self.socket.close()
        except:
            pass
        
        # Poczekaj na zakończenie wątku
        if self.server_thread and self.server_thread.is_alive():
            self.server_thread.join(timeout=1.0)
        
        # Zamknij context
        try:
            self.context.term()
        except:
            pass
        
        self.logger.info("Monitor Server zatrzymany")
    
    def __del__(self):
        self.stop()

if __name__ == "__main__":
    # Konfiguracja logowania
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    server = MonitorServer()
    
    try:
        server.start()
        
        # Utrzymaj serwer przy życiu
        while server.running:
            time.sleep(0.1)
            
    except KeyboardInterrupt:
        print("\n⚠️ Przerwano przez użytkownika")
    finally:
        server.stop()