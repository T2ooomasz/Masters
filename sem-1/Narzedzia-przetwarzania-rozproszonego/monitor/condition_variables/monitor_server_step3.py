#!/usr/bin/env python3
"""
Monitor Server - Etap 3
Distributed Monitor with Condition Variables

Serwer zarządza:
- Mutex (mutual exclusion)
- Kolejkami warunków (condition queues)
- Stanem oczekujących procesów

Protokół:
- ENTER_MONITOR -> GRANTED/DENIED
- EXIT_MONITOR -> OK
- WAIT_CONDITION -> OK (zwalnia mutex, czeka)
- SIGNAL_CONDITION -> OK (budzi jeden proces)
- BROADCAST_CONDITION -> OK (budzi wszystkie procesy)
"""

import zmq
import json
import time
import threading
from collections import defaultdict, deque
from typing import Dict, Set, Optional
import logging

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)

class MonitorServer:
    """
    Centralny serwer monitora zarządzający dostępem i synchronizacją.
    
    Stan serwera:
    - mutex_owner: ID procesu który obecnie ma mutex (None = wolny)
    - mutex_queue: kolejka procesów czekających na mutex
    - condition_queues: mapa {condition_name -> kolejka_procesów}
    - waiting_processes: procesy które wykonały wait() i czekają na signal
    """
    
    def __init__(self, port: int = 5555):
        self.port = port
        
        # Stan mutex - kto ma dostęp
        self.mutex_owner: Optional[str] = None
        self.mutex_queue: deque = deque()  # kolejka czekających na mutex
        
        # Kolejki warunków - {condition_name: [process_id, ...]}
        self.condition_queues: Dict[str, deque] = defaultdict(deque)
        
        # Procesy czekające na warunki (wykonały wait)
        self.waiting_processes: Set[str] = set()
        
        # ØMQ setup
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        self.socket.bind(f"tcp://*:{port}")
        
        # Synchronizacja dostępu do stanu serwera
        self.lock = threading.Lock()
        
        logger.info(f"Monitor Server uruchomiony na porcie {port}")
    
    def handle_enter_monitor(self, process_id: str) -> dict:
        """
        Żądanie wejścia do monitora.
        
        Logika:
        - Jeśli mutex wolny -> przydziel i GRANTED
        - Jeśli zajęty -> dodaj do kolejki i DENIED
        """
        with self.lock:
            if self.mutex_owner is None:
                # Mutex wolny - przydziel
                self.mutex_owner = process_id
                logger.info(f"ENTER: {process_id} otrzymał mutex")
                return {"status": "GRANTED"}
            else:
                # Mutex zajęty - dodaj do kolejki
                if process_id not in self.mutex_queue:
                    self.mutex_queue.append(process_id)
                logger.info(f"ENTER: {process_id} czeka w kolejce (pozycja {len(self.mutex_queue)})")
                return {"status": "DENIED", "queue_position": len(self.mutex_queue)}
    
    def handle_exit_monitor(self, process_id: str) -> dict:
        """
        Żądanie wyjścia z monitora.
        
        Logika:
        - Sprawdź czy proces rzeczywiście ma mutex
        - Zwolnij mutex
        - Przydziel następnemu w kolejce
        """
        with self.lock:
            if self.mutex_owner != process_id:
                logger.warning(f"EXIT: {process_id} próbuje zwolnić mutex nie będąc właścicielem")
                return {"status": "ERROR", "message": "Nie masz mutex"}
            
            # Zwolnij mutex
            self.mutex_owner = None
            logger.info(f"EXIT: {process_id} zwolnił mutex")
            
            # Przydziel następnemu w kolejce
            self._try_grant_mutex()
            
            return {"status": "OK"}
    
    def handle_wait_condition(self, process_id: str, condition: str) -> dict:
        """
        Proces czeka na warunek.
        
        Logika:
        - Sprawdź czy proces ma mutex
        - Dodaj proces do kolejki warunku
        - Zwolnij mutex
        - Przydziel mutex następnemu
        - Proces będzie czekał na signal
        """
        with self.lock:
            if self.mutex_owner != process_id:
                logger.warning(f"WAIT: {process_id} próbuje wait bez mutex")
                return {"status": "ERROR", "message": "Musisz mieć mutex żeby wait"}
            
            # Dodaj do kolejki warunku
            self.condition_queues[condition].append(process_id)
            self.waiting_processes.add(process_id)
            
            # Zwolnij mutex
            self.mutex_owner = None
            logger.info(f"WAIT: {process_id} czeka na '{condition}', zwolnił mutex")
            
            # Przydziel mutex następnemu
            self._try_grant_mutex()
            
            return {"status": "OK", "message": f"Czekasz na '{condition}'"}
    
    def handle_signal_condition(self, process_id: str, condition: str) -> dict:
        """
        Sygnalizuj warunek - obudź jeden proces.
        
        Logika:
        - Sprawdź czy proces ma mutex
        - Znajdź pierwszy proces czekający na ten warunek
        - Przenieś go z condition_queue do mutex_queue
        """
        with self.lock:
            if self.mutex_owner != process_id:
                logger.warning(f"SIGNAL: {process_id} próbuje signal bez mutex")
                return {"status": "ERROR", "message": "Musisz mieć mutex żeby signal"}
            
            # Sprawdź czy ktoś czeka na ten warunek
            if not self.condition_queues[condition]:
                logger.info(f"SIGNAL: {process_id} sygnalizuje '{condition}' - nikt nie czeka")
                return {"status": "OK", "message": f"Nikt nie czeka na '{condition}'"}
            
            # Obudź pierwszy proces z kolejki
            waiting_process = self.condition_queues[condition].popleft()
            self.waiting_processes.discard(waiting_process)
            
            # Dodaj go do kolejki mutex (na początek - ma priorytet)
            self.mutex_queue.appendleft(waiting_process)
            
            logger.info(f"SIGNAL: {process_id} obudził {waiting_process} z '{condition}'")
            return {"status": "OK", "message": f"Obudził {waiting_process}"}
    
    def handle_broadcast_condition(self, process_id: str, condition: str) -> dict:
        """
        Broadcast warunek - obudź wszystkie procesy.
        
        Logika:
        - Sprawdź czy proces ma mutex
        - Wszystkie procesy z condition_queue przenieś do mutex_queue
        """
        with self.lock:
            if self.mutex_owner != process_id:
                logger.warning(f"BROADCAST: {process_id} próbuje broadcast bez mutex")
                return {"status": "ERROR", "message": "Musisz mieć mutex żeby broadcast"}
            
            # Sprawdź czy ktoś czeka
            if not self.condition_queues[condition]:
                logger.info(f"BROADCAST: {process_id} broadcast '{condition}' - nikt nie czeka")
                return {"status": "OK", "message": f"Nikt nie czeka na '{condition}'"}
            
            # Obudź wszystkie procesy
            awakened = []
            while self.condition_queues[condition]:
                waiting_process = self.condition_queues[condition].popleft()
                self.waiting_processes.discard(waiting_process)
                self.mutex_queue.appendleft(waiting_process)  # priorytet
                awakened.append(waiting_process)
            
            logger.info(f"BROADCAST: {process_id} obudził {len(awakened)} procesów z '{condition}'")
            return {"status": "OK", "message": f"Obudził {len(awakened)} procesów"}
    
    def _try_grant_mutex(self):
        """
        Pomocnicza: spróbuj przydzielić mutex następnemu w kolejce.
        UWAGA: Wywołać tylko gdy self.lock jest trzymany!
        """
        if self.mutex_owner is None and self.mutex_queue:
            next_process = self.mutex_queue.popleft()
            self.mutex_owner = next_process
            logger.info(f"GRANT: {next_process} otrzymał mutex z kolejki")
    
    def get_status(self) -> dict:
        """Status serwera dla debugowania."""
        with self.lock:
            return {
                "mutex_owner": self.mutex_owner,
                "mutex_queue": list(self.mutex_queue),
                "condition_queues": {k: list(v) for k, v in self.condition_queues.items()},
                "waiting_processes": list(self.waiting_processes)
            }
    
    def handle_request(self, request: dict) -> dict:
        """
        Główna funkcja obsługi żądań.
        
        Protokół:
        {
            "action": "ENTER_MONITOR" | "EXIT_MONITOR" | "WAIT_CONDITION" | "SIGNAL_CONDITION" | "BROADCAST_CONDITION" | "STATUS",
            "process_id": "client_123",
            "condition": "condition_name"  # tylko dla wait/signal/broadcast
        }
        """
        action = request.get("action")
        process_id = request.get("process_id")
        condition = request.get("condition")
        
        logger.debug(f"Request: {action} from {process_id}")
        
        try:
            if action == "ENTER_MONITOR":
                return self.handle_enter_monitor(process_id)
            elif action == "EXIT_MONITOR":
                return self.handle_exit_monitor(process_id)
            elif action == "WAIT_CONDITION":
                return self.handle_wait_condition(process_id, condition)
            elif action == "SIGNAL_CONDITION":
                return self.handle_signal_condition(process_id, condition)
            elif action == "BROADCAST_CONDITION":
                return self.handle_broadcast_condition(process_id, condition)
            elif action == "STATUS":
                return {"status": "OK", "server_status": self.get_status()}
            else:
                return {"status": "ERROR", "message": f"Nieznana akcja: {action}"}
        
        except Exception as e:
            logger.error(f"Błąd obsługi żądania {action}: {e}")
            return {"status": "ERROR", "message": str(e)}
    
    def run(self):
        """Główna pętla serwera."""
        logger.info("Monitor Server rozpoczyna obsługę żądań...")
        
        try:
            while True:
                # Odbierz żądanie (blokujące)
                message = self.socket.recv_string()
                logger.debug(f"Otrzymano: {message}")
                
                # Parsuj JSON
                try:
                    request = json.loads(message)
                except json.JSONDecodeError:
                    response = {"status": "ERROR", "message": "Nieprawidłowy JSON"}
                else:
                    response = self.handle_request(request)
                
                # Wyślij odpowiedź
                response_json = json.dumps(response)
                self.socket.send_string(response_json)
                logger.debug(f"Wysłano: {response_json}")
        
        except KeyboardInterrupt:
            logger.info("Serwer zatrzymany przez użytkownika")
        except Exception as e:
            logger.error(f"Błąd serwera: {e}")
        finally:
            self.socket.close()
            self.context.term()

if __name__ == "__main__":
    server = MonitorServer(port=5555)
    
    # Wyświetl status co 10 sekund (opcjonalnie)
    def print_status():
        while True:
            time.sleep(10)
            status = server.get_status()
            if any([status["mutex_owner"], status["mutex_queue"], status["condition_queues"]]):
                logger.info(f"Status: {status}")
    
    status_thread = threading.Thread(target=print_status, daemon=True)
    status_thread.start()
    
    # Uruchom serwer
    server.run()