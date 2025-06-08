#!/usr/bin/env python3
"""
Monitor Client - Etap 3
Distributed Monitor Client Library

API transparentne jak zwykły monitor:
- enter() - wejście do monitora
- exit() - wyjście z monitora
- wait(condition) - czekaj na warunek
- signal(condition) - obudź jeden proces
- broadcast(condition) - obudź wszystkie procesy

Użycie:
monitor = MonitorClient("tcp://localhost:5555")
monitor.enter()
try:
    while not condition:
        monitor.wait("condition_name")
    # sekcja krytyczna
    monitor.signal("other_condition")
finally:
    monitor.exit()
"""

import zmq
import json
import time
import uuid
import logging
from typing import Optional

# Konfiguracja logowania
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class MonitorClient:
    """
    Klient distributed monitora.
    
    Zapewnia transparentne API podobne do lokalnych monitorów.
    Komunikuje się z MonitorServer przez ØMQ REQ-REP.
    """
    
    def __init__(self, server_address: str = "tcp://localhost:5555"):
        self.server_address = server_address
        
        # Unikalny ID tego procesu/klienta
        self.process_id = f"client_{uuid.uuid4().hex[:8]}"
        
        # ØMQ setup
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REQ)
        self.socket.connect(server_address)
        
        # Stan klienta
        self.has_mutex = False
        self.is_waiting = False
        
        logger.info(f"Monitor Client {self.process_id} połączony z {server_address}")
    
    def _send_request(self, action: str, condition: Optional[str] = None) -> dict:
        """
        Wyślij żądanie do serwera i otrzymaj odpowiedź.
        
        Args:
            action: Typ akcji (ENTER_MONITOR, EXIT_MONITOR, etc.)
            condition: Nazwa warunku (dla wait/signal/broadcast)
        
        Returns:
            Odpowiedź serwera jako dict
        """
        request = {
            "action": action,
            "process_id": self.process_id
        }
        
        if condition is not None:
            request["condition"] = condition
        
        # Wyślij żądanie
        request_json = json.dumps(request)
        logger.debug(f"Wysyłam: {request_json}")
        self.socket.send_string(request_json)
        
        # Odbierz odpowiedź
        response_json = self.socket.recv_string()
        logger.debug(f"Otrzymałem: {response_json}")
        response = json.loads(response_json)
        
        return response
    
    def enter(self, timeout: Optional[float] = None) -> bool:
        """
        Wejdź do monitora (uzyskaj mutex).
        
        Blokuje dopóki nie otrzyma dostępu.
        
        Args:
            timeout: Maksymalny czas oczekiwania w sekundach (None = bez limitu)
        
        Returns:
            True jeśli uzyskano dostęp, False przy timeout
        """
        if self.has_mutex:
            logger.warning("Już mam mutex - enter() ignorowane")
            return True
        
        start_time = time.time()
        
        while True:
            response = self._send_request("ENTER_MONITOR")
            
            if response["status"] == "GRANTED":
                self.has_mutex = True
                logger.info(f"Otrzymałem mutex")
                return True
            elif response["status"] == "DENIED":
                # Czekamy w kolejce
                queue_pos = response.get("queue_position", "?")
                logger.info(f"Czekam na mutex (pozycja w kolejce: {queue_pos})")
                
                # Sprawdź timeout
                if timeout is not None:
                    elapsed = time.time() - start_time
                    if elapsed >= timeout:
                        logger.warning(f"Timeout podczas oczekiwania na mutex")
                        return False
                
                # Poczekaj przed ponownym sprawdzeniem
                time.sleep(0.1)
            else:
                logger.error(f"Błąd podczas enter(): {response}")
                return False
    
    def exit(self):
        """
        Wyjdź z monitora (zwolnij mutex).
        """
        if not self.has_mutex:
            logger.warning("Nie mam mutex - exit() ignorowane")
            return
        
        response = self._send_request("EXIT_MONITOR")
        
        if response["status"] == "OK":
            self.has_mutex = False
            logger.info("Zwolniłem mutex")
        else:
            logger.error(f"Błąd podczas exit(): {response}")
    
    def wait(self, condition: str, timeout: Optional[float] = None) -> bool:
        """
        Czekaj na warunek.
        
        Zwalnia mutex i czeka aż inny proces wywoła signal/broadcast.
        Po obudzeniu ponownie uzyskuje mutex.
        
        Args:
            condition: Nazwa warunku do oczekiwania
            timeout: Maksymalny czas oczekiwania w sekundach
        
        Returns:
            True jeśli obudzony przez signal, False przy timeout/błędzie
        """
        if not self.has_mutex:
            logger.error("Musisz mieć mutex żeby wait()")
            return False
        
        # Poinformuj serwer że czekamy
        response = self._send_request("WAIT_CONDITION", condition)
        
        if response["status"] != "OK":
            logger.error(f"Błąd podczas wait(): {response}")
            return False
        
        # Mutex został zwolniony przez serwer
        self.has_mutex = False
        self.is_waiting = True
        logger.info(f"Czekam na warunek '{condition}'")
        
        # Czekaj na ponowne uzyskanie mutex (po signal/broadcast)
        start_time = time.time()
        
        while self.is_waiting:
            # Sprawdź czy możemy ponownie uzyskać mutex
            response = self._send_request("ENTER_MONITOR")
            
            if response["status"] == "GRANTED":
                self.has_mutex = True
                self.is_waiting = False
                logger.info(f"Obudzony z warunku '{condition}', mam mutex")
                return True
            
            # Sprawdź timeout
            if timeout is not None:
                elapsed = time.time() - start_time
                if elapsed >= timeout:
                    self.is_waiting = False
                    logger.warning(f"Timeout podczas wait na '{condition}'")
                    return False
            
            # Poczekaj przed ponownym sprawdzeniem
            time.sleep(0.1)
        
        return False
    
    def signal(self, condition: str):
        """
        Sygnalizuj warunek - obudź jeden czekający proces.
        
        Args:
            condition: Nazwa warunku do sygnalizacji
        """
        if not self.has_mutex:
            logger.error("Musisz mieć mutex żeby signal()")
            return
        
        response = self._send_request("SIGNAL_CONDITION", condition)
        
        if response["status"] == "OK":
            logger.info(f"Sygnalizowałem warunek '{condition}': {response.get('message', '')}")
        else:
            logger.error(f"Błąd podczas signal(): {response}")
    
    def broadcast(self, condition: str):
        """
        Broadcast warunek - obudź wszystkie czekające procesy.
        
        Args:
            condition: Nazwa warunku do broadcast
        """
        if not self.has_mutex:
            logger.error("Musisz mieć mutex żeby broadcast()")
            return
        
        response = self._send_request("BROADCAST_CONDITION", condition)
        
        if response["status"] == "OK":
            logger.info(f"Broadcast warunek '{condition}': {response.get('message', '')}")
        else:
            logger.error(f"Błąd podczas broadcast(): {response}")
    
    def get_server_status(self) -> dict:
        """Pobierz status serwera (do debugowania)."""
        response = self._send_request("STATUS")
        return response.get("server_status", {})
    
    def close(self):
        """Zamknij połączenie z serwerem."""
        if self.has_mutex:
            logger.warning("Zamykam połączenie ale nadal mam mutex - wywoływam exit()")
            self.exit()
        
        self.socket.close()
        self.context.term()
        logger.info("Połączenie zamknięte")
    
    def __enter__(self):
        """Context manager support."""
        self.enter()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager support."""
        self.exit()

# Przykład użycia
if __name__ == "__main__":
    # Test podstawowy
    print("=== Test Monitor Client ===")
    
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        print("1. Wchodzę do monitora...")
        if monitor.enter(timeout=5.0):
            print("   ✓ Mam mutex")
            
            print("2. Testuję signal (nikt nie czeka)...")
            monitor.signal("test_condition")
            
            print("3. Wychodzę z monitora...")
            monitor.exit()
            print("   ✓ Mutex zwolniony")
        else:
            print("   ✗ Timeout podczas enter()")
    
    except Exception as e:
        print(f"Błąd: {e}")
    finally:
        monitor.close()
    
    print("\n=== Test Context Manager ===")
    
    # Test z context manager
    try:
        with MonitorClient("tcp://localhost:5555") as monitor:
            print("Automatyczne enter() - mam mutex")
            monitor.signal("context_test")
        print("Automatyczne exit() - mutex zwolniony")
    except Exception as e:
        print(f"Błąd: {e}")