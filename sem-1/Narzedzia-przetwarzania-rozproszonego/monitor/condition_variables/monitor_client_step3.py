#!/usr/bin/env python3
"""
Monitor Client - Etap 3: Condition Variables
Biblioteka klienta do komunikacji z distributed monitor.

Oferuje transparentne API podobne do standardowych monitorów:
- enter() / exit() - wejście/wyjście z monitora
- wait(condition) - oczekiwanie na warunek
- signal(condition) - sygnalizacja warunku (budzi jeden proces)
- broadcast(condition) - broadcast warunku (budzi wszystkie procesy)

Użycie:
    monitor = DistributedMonitor("tcp://localhost:5555")
    monitor.enter()
    try:
        while not ready:
            monitor.wait("data_ready")
        # sekcja krytyczna
        monitor.signal("space_available")
    finally:
        monitor.exit()
"""

import zmq
import json
import uuid
import time
import logging
from typing import Optional, Dict, Any
from contextlib import contextmanager

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('MonitorClient')

class DistributedMonitorError(Exception):
    """Wyjątek specyficzny dla distributed monitor"""
    pass

class DistributedMonitor:
    """
    Klient distributed monitor - transparentne API dla synchronizacji rozproszonej.
    
    Główne założenia:
    - Każdy klient ma unikalny ID
    - Komunikacja przez REQ-REP zapewnia niezawodność
    - API przypomina standardowe monitory
    - Automatyczne zarządzanie połączeniem
    """
    
    def __init__(self, server_address: str = "tcp://localhost:5555", timeout: int = 5000):
        """
        Inicjalizacja klienta monitora.
        
        Args:
            server_address: Adres serwera monitora
            timeout: Timeout dla operacji ZMQ (ms)
        """
        self.server_address = server_address
        self.timeout = timeout
        self.client_id = f"client_{uuid.uuid4().hex[:8]}"
        
        # Stan klienta
        self.has_mutex = False
        self.is_connected = False
        
        # ZMQ setup
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REQ)
        self.socket.setsockopt(zmq.RCVTIMEO, timeout)
        self.socket.setsockopt(zmq.SNDTIMEO, timeout)
        
        logger.info(f"Klient {self.client_id} utworzony")
    
    def connect(self):
        """Nawiązanie połączenia z serwerem"""
        if not self.is_connected:
            self.socket.connect(self.server_address)
            self.is_connected = True
            logger.info(f"Klient {self.client_id} połączony z {self.server_address}")
    
    def disconnect(self):
        """Rozłączenie z serwerem"""
        if self.is_connected:
            if self.has_mutex:
                logger.warning(f"Klient {self.client_id} rozłącza się mając mutex - wymuszam exit")
                try:
                    self.exit()
                except:
                    pass
            
            self.socket.close()
            self.context.term()
            self.is_connected = False
            logger.info(f"Klient {self.client_id} rozłączony")
    
    def _send_request(self, action: str, **kwargs) -> Dict[str, Any]:
        """
        Wysłanie żądania do serwera i oczekiwanie na odpowiedź.
        
        Args:
            action: Typ akcji (ENTER_MONITOR, EXIT_MONITOR, etc.)
            **kwargs: Dodatkowe parametry żądania
            
        Returns:
            Odpowiedź serwera jako dict
            
        Raises:
            DistributedMonitorError: W przypadku błędu komunikacji lub odmowy serwera
        """
        if not self.is_connected:
            self.connect()
        
        # Przygotowanie żądania
        request = {
            'action': action,
            'client_id': self.client_id,
            'timestamp': time.time(),
            **kwargs
        }
        
        try:
            # Wysłanie żądania
            self.socket.send_json(request)
            logger.debug(f"Wysłano żądanie: {action}")
            
            # Oczekiwanie na odpowiedź
            response = self.socket.recv_json()
            logger.debug(f"Otrzymano odpowiedź: {response}")
            
            return response
            
        except zmq.Again:
            raise DistributedMonitorError(f"Timeout podczas komunikacji z serwerem ({self.timeout}ms)")
        except Exception as e:
            raise DistributedMonitorError(f"Błąd komunikacji: {e}")
    
    def enter(self) -> bool:
        """
        Wejście do monitora (acquire mutex).
        
        Semantyka:
        - Blokuje dopóki mutex nie zostanie przyznany
        - Zwraca True gdy mutex zostanie przyznany
        
        Returns:
            True gdy mutex przyznany
            
        Raises:
            DistributedMonitorError: W przypadku błędu
        """
        if self.has_mutex:
            raise DistributedMonitorError("Klient już ma mutex")
        
        logger.info(f"Klient {self.client_id} próbuje wejść do monitora")
        
        while True:
            response = self._send_request('ENTER_MONITOR')
            
            if response['status'] == 'GRANTED':
                self.has_mutex = True
                logger.info(f"Klient {self.client_id} wszedł do monitora")
                return True
                
            elif response['status'] == 'QUEUED':
                position = response.get('position', '?')
                logger.info(f"Klient {self.client_id} w kolejce (pozycja {position})")
                time.sleep(0.1)  # Krótkie oczekiwanie przed ponowną próbą
                
            elif response['status'] == 'ERROR':
                raise DistributedMonitorError(f"Błąd wejścia: {response.get('message', 'Nieznany błąd')}")
            
            else:
                raise DistributedMonitorError(f"Nieoczekiwana odpowiedź: {response}")
    
    def exit(self) -> bool:
        """
        Wyjście z monitora (release mutex).
        
        Returns:
            True gdy mutex został zwolniony
            
        Raises:
            DistributedMonitorError: W przypadku błędu
        """
        if not self.has_mutex:
            raise DistributedMonitorError("Klient nie ma mutex")
        
        logger.info(f"Klient {self.client_id} wychodzi z monitora")
        
        response = self._send_request('EXIT_MONITOR')
        
        if response['status'] == 'RELEASED':
            self.has_mutex = False
            logger.info(f"Klient {self.client_id} wyszedł z monitora")
            return True
        elif response['status'] == 'ERROR':
            raise DistributedMonitorError(f"Błąd wyjścia: {response.get('message', 'Nieznany błąd')}")
        else:
            raise DistributedMonitorError(f"Nieoczekiwana odpowiedź: {response}")
    
    def wait(self, condition: str):
        """
        Oczekiwanie na warunek.
        
        Kluczowa semantyka monitora:
        1. Klient MUSI mieć mutex
        2. wait() ZWALNIA mutex i blokuje klienta
        3. Klient zostaje obudzony przez signal() lub broadcast()
        4. Po obudzeniu klient MUSI ponownie zdobyć mutex
        
        Args:
            condition: Nazwa warunku do oczekiwania
            
        Raises:
            DistributedMonitorError: W przypadku błędu
        """
        if not self.has_mutex:
            raise DistributedMonitorError("Musisz mieć mutex żeby wykonać wait")
        
        logger.info(f"Klient {self.client_id} czeka na warunek '{condition}'")
        
        # Wyślij żądanie wait - to ZWALNIA mutex
        response = self._send_request('WAIT_CONDITION', condition=condition)
        
        if response['status'] == 'WAITING':
            self.has_mutex = False  # Mutex został zwolniony
            logger.info(f"Klient {self.client_id} oczekuje na warunek '{condition}'")
            
            # Teraz blokujemy w oczekiwaniu na obudzenie i ponowne zdobycie mutex
            self._wait_for_wakeup()
            
        elif response['status'] == 'ERROR':
            raise DistributedMonitorError(f"Błąd wait: {response.get('message', 'Nieznany błąd')}")
        else:
            raise DistributedMonitorError(f"Nieoczekiwana odpowiedź: {response}")
    
    def _wait_for_wakeup(self):
        """
        Oczekiwanie na obudzenie i ponowne zdobycie mutex.
        
        Po wywołaniu wait(), klient musi:
        1. Czekać aż zostanie obudzony przez signal/broadcast
        2. Ponownie zdobyć mutex (może czekać w kolejce)
        """
        logger.info(f"Klient {self.client_id} oczekuje na obudzenie")
        
        # Cykliczne sprawdzanie czy mamy już mutex (zostaliśmy obudzeni)
        while not self.has_mutex:
            response = self._send_request('ENTER_MONITOR')
            
            if response['status'] == 'GRANTED':
                self.has_mutex = True
                logger.info(f"Klient {self.client_id} obudzony i ponownie ma mutex")
                break
                
            elif response['status'] == 'QUEUED':
                # Wciąż czekamy
                time.sleep(0.1)
                
            elif response['status'] == 'ERROR':
                raise DistributedMonitorError(f"Błąd podczas oczekiwania na obudzenie: {response.get('message')}")
    
    def signal(self, condition: str):
        """
        Sygnalizacja warunku - budzi JEDEN oczekujący proces.
        
        Args:
            condition: Nazwa warunku do sygnalizacji
            
        Raises:
            DistributedMonitorError: W przypadku błędu
        """
        if not self.has_mutex:
            raise DistributedMonitorError("Musisz mieć mutex żeby sygnalizować")
        
        logger.info(f"Klient {self.client_id} sygnalizuje warunek '{condition}'")
        
        response = self._send_request('SIGNAL_CONDITION', condition=condition)
        
        if response['status'] == 'SIGNALED':
            woken = response.get('woken_processes', 0)
            logger.info(f"Sygnalizacja '{condition}': obudzono {woken} procesów")
        elif response['status'] == 'ERROR':
            raise DistributedMonitorError(f"Błąd signal: {response.get('message', 'Nieznany błąd')}")
    
    def broadcast(self, condition: str):
        """
        Broadcast warunku - budzi WSZYSTKIE oczekujące procesy.
        
        Args:
            condition: Nazwa warunku do broadcast
            
        Raises:
            DistributedMonitorError: W przypadku błędu
        """
        if not self.has_mutex:
            raise DistributedMonitorError("Musisz mieć mutex żeby broadcastować")
        
        logger.info(f"Klient {self.client_id} broadcastuje warunek '{condition}'")
        
        response = self._send_request('BROADCAST_CONDITION', condition=condition)
        
        if response['status'] == 'BROADCASTED':
            woken = response.get('woken_processes', 0)
            logger.info(f"Broadcast '{condition}': obudzono {woken} procesów")
        elif response['status'] == 'ERROR':
            raise DistributedMonitorError(f"Błąd broadcast: {response.get('message', 'Nieznany błąd')}")
    
    def get_server_status(self) -> Dict[str, Any]:
        """Pobranie stanu serwera (do debugowania)"""
        response = self._send_request('GET_STATUS')
        return response
    
    @contextmanager
    def synchronized(self):
        """
        Context manager dla transparentnego użycia monitora.
        
        Użycie:
            with monitor.synchronized():
                # sekcja krytyczna
                while not ready:
                    monitor.wait("condition")
                monitor.signal("other_condition")
        """
        self.enter()
        try:
            yield
        finally:
            self.exit()
    
    def __enter__(self):
        """Support for 'with' statement"""
        self.enter()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Support for 'with' statement"""
        self.exit()
    
    def __del__(self):
        """Cleanup przy niszczeniu obiektu"""
        try:
            self.disconnect()
        except:
            pass

# Przykład użycia
def example_usage():
    """Demonstracja użycia DistributedMonitor"""
    
    monitor = DistributedMonitor("tcp://localhost:5555")
    
    try:
        # Sposób 1: Explicit enter/exit
        monitor.enter()
        try:
            print("W sekcji krytycznej")
            time.sleep(1)
            # monitor.wait("some_condition")  # Przykład wait
            # monitor.signal("other_condition")  # Przykład signal
        finally:
            monitor.exit()
        
        # Sposób 2: Context manager
        with monitor.synchronized():
            print("W sekcji krytycznej (context manager)")
            time.sleep(1)
        
        # Sposób 3: With statement
        with monitor:
            print("W sekcji krytycznej (with statement)")
            time.sleep(1)
            
    finally:
        monitor.disconnect()

if __name__ == "__main__":
    example_usage()