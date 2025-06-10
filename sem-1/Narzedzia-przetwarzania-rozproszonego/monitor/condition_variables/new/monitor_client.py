#!/usr/bin/env python3
"""
Distributed Monitor Client - Klient monitora rozproszonego
Zapewnia transparentne API podobne do standardowego monitora
"""

import zmq
import json
import time
import uuid
import logging
from typing import Optional, Dict, Any
import threading

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger('MonitorClient')

class MonitorError(Exception):
    """Wyjątek dla błędów monitora"""
    pass

class MonitorClient:
    """Klient monitora rozproszonego"""
    
    def __init__(self, server_url: str = "tcp://localhost:5555"):
        self.server_url = server_url
        self.client_id = f"client_{uuid.uuid4().hex[:8]}"
        
        # ZMQ setup
        self.context = None
        self.socket = None
        self.connected = False
        
        # Stan klienta
        self.in_monitor = False
        self.waiting_for = None  # Nazwa warunku na który oczekujemy
        
        # Thread safety dla operacji klienta
        self.lock = threading.Lock()
        
        logger.info(f"Klient {self.client_id} utworzony")
    
    def __enter__(self):
        """Context manager - wejście"""
        self.enter()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager - wyjście"""
        if self.in_monitor:
            self.exit()
        self.disconnect()
    
    def connect(self):
        """Połączenie z serwerem"""
        if self.connected:
            return
        
        try:
            self.context = zmq.Context()
            self.socket = self.context.socket(zmq.REQ)
            self.socket.connect(self.server_url)
            self.connected = True
            logger.info(f"Klient {self.client_id} połączony z {self.server_url}")
            
        except Exception as e:
            logger.error(f"Błąd połączenia: {e}")
            raise MonitorError(f"Nie można połączyć się z serwerem: {e}")
    
    def disconnect(self):
        """Rozłączenie z serwerem"""
        if not self.connected:
            return
        
        try:
            if self.socket:
                self.socket.close()
            if self.context:
                self.context.term()
            
            self.connected = False
            self.socket = None
            self.context = None
            
            logger.info(f"Klient {self.client_id} rozłączony")
            
        except Exception as e:
            logger.error(f"Błąd rozłączania: {e}")
    
    def _send_command(self, command: str, **kwargs) -> Dict[str, Any]:
        """Wysłanie komendy do serwera"""
        if not self.connected:
            self.connect()
        
        message = {
            'command': command,
            'client_id': self.client_id,
            **kwargs
        }
        
        try:
            self.socket.send_json(message)
            response = self.socket.recv_json()
            return response
            
        except zmq.ZMQError as e:
            logger.error(f"Błąd komunikacji ZMQ: {e}")
            raise MonitorError(f"Błąd komunikacji z serwerem: {e}")
        except Exception as e:
            logger.error(f"Błąd wysyłania komendy: {e}")
            raise MonitorError(f"Błąd komunikacji: {e}")
    
    def enter(self):
        """Wejście do monitora - acquire mutex"""
        with self.lock:
            if self.in_monitor:
                raise MonitorError("Klient już jest w monitorze")
            
            logger.info(f"Klient {self.client_id} próbuje wejść do monitora")
            
            # Wysłanie żądania wejścia
            response = self._send_command('ENTER_MONITOR')
            
            if response['status'] == 'granted':
                self.in_monitor = True
                logger.info(f"Klient {self.client_id} wszedł do monitora")
                
            elif response['status'] == 'waiting':
                # Oczekujemy na dostęp - blokujemy do momentu przyznania
                self._wait_for_mutex_grant()
                
            elif response['status'] == 'error':
                raise MonitorError(f"Błąd wejścia do monitora: {response['message']}")
            else:
                raise MonitorError(f"Nieoczekiwana odpowiedź: {response}")
    
    def _wait_for_mutex_grant(self):
        """Oczekiwanie na przyznanie mutex (dla kolejki)"""
        # W rzeczywistej implementacji należałoby użyć mechanizmu callback
        # lub dodatkowego socketu dla powiadomień push/pull
        # Na potrzeby testów implementujemy prostsze rozwiązanie - polling
        
        while not self.in_monitor:
            time.sleep(0.1)  # Krótka przerwa
            
            # Sprawdź status - czy już mamy dostęp
            try:
                status_response = self._send_command('GET_STATUS')
                if (status_response['status'] == 'success' and 
                    status_response['server_status']['mutex_owner'] == self.client_id):
                    
                    self.in_monitor = True
                    logger.info(f"Klient {self.client_id} wszedł do monitora")
                    break
                    
            except Exception as e:
                logger.error(f"Błąd sprawdzania statusu: {e}")
                time.sleep(0.5)  # Dłuższa przerwa przy błędzie
    
    def exit(self):
        """Wyjście z monitora - release mutex"""
        with self.lock:
            if not self.in_monitor:
                raise MonitorError("Klient nie jest w monitorze")
            
            logger.info(f"Klient {self.client_id} wychodzi z monitora")
            
            response = self._send_command('EXIT_MONITOR')
            
            if response['status'] == 'success':
                self.in_monitor = False
                self.waiting_for = None
                logger.info(f"Klient {self.client_id} wyszedł z monitora")
            else:
                raise MonitorError(f"Błąd wyjścia z monitora: {response['message']}")
    
    def wait(self, condition: str):
        """Oczekiwanie na warunek"""
        with self.lock:
            if not self.in_monitor:
                raise MonitorError("Klient nie jest w monitorze - nie można wait")
            
            logger.info(f"Klient {self.client_id} oczekuje na warunek '{condition}'")
            
            response = self._send_command('WAIT_CONDITION', condition=condition)
            
            if response['status'] == 'waiting':
                self.in_monitor = False  # Wait zwalnia mutex
                self.waiting_for = condition
                
                # Oczekuj na obudzenie i ponowne przyznanie mutex
                self._wait_for_signal()
                
            elif response['status'] == 'error':
                raise MonitorError(f"Błąd wait: {response['message']}")
            else:
                raise MonitorError(f"Nieoczekiwana odpowiedź wait: {response}")
    
    def _wait_for_signal(self):
        """Oczekiwanie na sygnał (signal/broadcast)"""
        # Podobnie jak w _wait_for_mutex_grant - polling status
        while self.waiting_for is not None:
            time.sleep(0.1)
            
            try:
                status_response = self._send_command('GET_STATUS')
                if (status_response['status'] == 'success' and 
                    status_response['server_status']['mutex_owner'] == self.client_id):
                    
                    # Zostaliśmy obudzeni i dostaliśmy mutex z powrotem
                    self.in_monitor = True
                    self.waiting_for = None
                    logger.info(f"Klient {self.client_id} obudzony i wrócił do monitora")
                    break
                    
            except Exception as e:
                logger.error(f"Błąd sprawdzania statusu podczas wait: {e}")
                time.sleep(0.5)
    
    def signal(self, condition: str):
        """Sygnalizacja warunku - obudź jeden proces"""
        with self.lock:
            if not self.in_monitor:
                raise MonitorError("Klient nie jest w monitorze - nie można signal")
            
            logger.info(f"Klient {self.client_id} sygnalizuje warunek '{condition}'")
            
            response = self._send_command('SIGNAL_CONDITION', condition=condition)
            
            if response['status'] != 'success':
                raise MonitorError(f"Błąd signal: {response['message']}")
            
            logger.info(f"Signal '{condition}' wysłany")
    
    def broadcast(self, condition: str):
        """Rozgłaszanie warunku - obudź wszystkie procesy"""
        with self.lock:
            if not self.in_monitor:
                raise MonitorError("Klient nie jest w monitorze - nie można broadcast")
            
            logger.info(f"Klient {self.client_id} rozgłasza warunek '{condition}'")
            
            response = self._send_command('BROADCAST_CONDITION', condition=condition)
            
            if response['status'] != 'success':
                raise MonitorError(f"Błąd broadcast: {response['message']}")
            
            awakened = response.get('awakened_count', 0)
            logger.info(f"Broadcast '{condition}' obudził {awakened} procesów")
    
    def get_status(self) -> Dict[str, Any]:
        """Pobieranie stanu serwera (do debugowania)"""
        response = self._send_command('GET_STATUS')
        if response['status'] == 'success':
            return response['server_status']
        else:
            raise MonitorError(f"Błąd pobierania statusu: {response['message']}")

# Context manager dla wygody użycia
class synchronized:
    """Context manager dla synchronizacji w monitorze"""
    
    def __init__(self, monitor: MonitorClient):
        self.monitor = monitor
    
    def __enter__(self):
        self.monitor.enter()
        return self.monitor
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.monitor.exit()

# Funkcje pomocnicze do testów
def create_monitor_client(server_url: str = "tcp://localhost:5555") -> MonitorClient:
    """Tworzenie klienta monitora"""
    return MonitorClient(server_url)

def test_basic_monitor():
    """Podstawowy test monitora"""
    print("🔧 TEST: Podstawowy monitor")
    
    client = create_monitor_client()
    
    try:
        client.connect()
        
        # Test enter/exit
        client.enter()
        print("✓ Wszedł do monitora")
        
        client.exit()
        print("✓ Wyszedł z monitora")
        
        print("✓ Test podstawowy zakończony pomyślnie")
        
    finally:
        client.disconnect()

if __name__ == "__main__":
    # Jeśli uruchomiony bezpośrednio, wykonaj test
    test_basic_monitor()