import zmq
import uuid
import time
import logging
from typing import Optional

class MonitorClient:
    def __init__(self, server_address="tcp://localhost:5555"):
        self.server_address = server_address
        self.client_id = f"client_{uuid.uuid4().hex[:8]}"
        
        # ZMQ setup
        self.context = zmq.Context()
        self.socket = None
        self.connected = False
        
        # Stan klienta
        self.in_monitor = False
        self.waiting_for_condition = None
        
        # Logging
        self.logger = logging.getLogger('MonitorClient')
        self.logger.info(f"Klient {self.client_id} utworzony")
    
    def _connect(self):
        """Połączenie z serwerem"""
        if self.connected:
            return
        
        self.socket = self.context.socket(zmq.REQ)
        self.socket.connect(self.server_address)
        self.connected = True
        self.logger.info(f"Klient {self.client_id} połączony z {self.server_address}")
    
    def _disconnect(self):
        """Rozłączenie z serwerem"""
        if not self.connected:
            return
        
        if self.socket:
            self.socket.close()
            self.socket = None
        
        self.connected = False
        self.logger.info(f"Klient {self.client_id} rozłączony")
    
    def _send_message(self, message, timeout_ms=5000):
        """Wysłanie wiadomości do serwera z timeout"""
        if not self.connected:
            self._connect()
        
        # Dodaj client_id do wiadomości
        message['client_id'] = self.client_id
        
        try:
            self.socket.send_json(message)
            
            # Oczekiwanie na odpowiedź z timeout
            if self.socket.poll(timeout_ms, zmq.POLLIN):
                response = self.socket.recv_json(zmq.NOBLOCK)
                return response
            else:
                raise TimeoutError(f"Brak odpowiedzi w ciągu {timeout_ms}ms")
                
        except zmq.ZMQError as e:
            self.logger.error(f"Błąd komunikacji: {e}")
            raise
    
    def enter(self, polling_interval=0.1, max_wait_time=30):
        """
        Wejście do monitora
        
        Args:
            polling_interval: Czas między sprawdzeniami kolejki (sekundy)
            max_wait_time: Maksymalny czas oczekiwania (sekundy)
        """
        if self.in_monitor:
            raise RuntimeError("Klient już jest w monitorze")
        
        self.logger.info(f"Klient {self.client_id} próbuje wejść do monitora")
        
        start_time = time.time()
        
        while time.time() - start_time < max_wait_time:
            try:
                response = self._send_message({'type': 'ENTER_MONITOR'})
                
                if response['type'] == 'MUTEX_GRANTED':
                    self.in_monitor = True
                    self.logger.info(f"Klient {self.client_id} wszedł do monitora")
                    return
                
                elif response['type'] == 'QUEUE_POSITION':
                    position = response['position']
                    self.logger.info(f"Klient {self.client_id} w kolejce (pozycja {position})")
                    
                    # Poczekaj przed kolejnym sprawdzeniem
                    time.sleep(polling_interval)
                    continue
                
                elif response['type'] == 'ERROR':
                    error_msg = response['message']
                    self.logger.error(f"Błąd wejścia do monitora: {error_msg}")
                    raise RuntimeError(f"Błąd wejścia do monitora: {error_msg}")
                
                else:
                    self.logger.warning(f"Nieoczekiwana odpowiedź: {response}")
                    time.sleep(polling_interval)
                    
            except TimeoutError:
                self.logger.warning("Timeout podczas oczekiwania na odpowiedź serwera")
                time.sleep(polling_interval)
                continue
            except Exception as e:
                self.logger.error(f"Błąd podczas wejścia do monitora: {e}")
                raise
        
        raise TimeoutError(f"Nie udało się wejść do monitora w ciągu {max_wait_time} sekund")
    
    def exit(self):
        """Wyjście z monitora"""
        if not self.in_monitor:
            raise RuntimeError("Klient nie jest w monitorze")
        
        self.logger.info(f"Klient {self.client_id} wychodzi z monitora")
        
        try:
            response = self._send_message({'type': 'EXIT_MONITOR'})
            
            if response['type'] == 'EXIT_CONFIRMED':
                self.in_monitor = False
                self.waiting_for_condition = None
                self.logger.info(f"Klient {self.client_id} wyszedł z monitora")
            elif response['type'] == 'ERROR':
                error_msg = response['message']
                self.logger.error(f"Błąd wyjścia z monitora: {error_msg}")
                raise RuntimeError(f"Błąd wyjścia z monitora: {error_msg}")
            else:
                self.logger.warning(f"Nieoczekiwana odpowiedź przy wyjściu: {response}")
                
        except Exception as e:
            self.logger.error(f"Błąd podczas wyjścia z monitora: {e}")
            # W przypadku błędu komunikacji, załóż że wyszliśmy
            self.in_monitor = False
            self.waiting_for_condition = None
            raise
    
    def wait(self, condition, timeout=None):
        """
        Oczekiwanie na warunek
        
        Args:
            condition: Nazwa warunku
            timeout: Maksymalny czas oczekiwania (None = bez limitu)
        """
        if not self.in_monitor:
            raise RuntimeError("Klient musi być w monitorze aby czekać")
        
        if self.waiting_for_condition is not None:
            raise RuntimeError("Klient już czeka na warunek")
        
        self.logger.info(f"Klient {self.client_id} czeka na warunek '{condition}'")
        
        # Wyślij żądanie wait
        response = self._send_message({
            'type': 'WAIT_CONDITION',
            'condition': condition
        })
        
        if response['type'] == 'WAIT_CONFIRMED':
            self.waiting_for_condition = condition
            self.in_monitor = False  # Zwolniliśmy mutex
            
            # Teraz oczekuj na ponowne przyznanie mutex
            self.enter(max_wait_time=timeout if timeout else 30)
            self.waiting_for_condition = None
            
        elif response['type'] == 'ERROR':
            error_msg = response['message']
            raise RuntimeError(f"Błąd wait: {error_msg}")
    
    def signal(self, condition):
        """Sygnalizacja warunku"""
        if not self.in_monitor:
            raise RuntimeError("Klient musi być w monitorze aby sygnalizować")
        
        response = self._send_message({
            'type': 'SIGNAL_CONDITION',
            'condition': condition
        })
        
        if response['type'] == 'SIGNAL_CONFIRMED':
            woken = response['woken']
            self.logger.info(f"Signal '{condition}' - obudzono {woken} klientów")
        elif response['type'] == 'ERROR':
            error_msg = response['message']
            raise RuntimeError(f"Błąd signal: {error_msg}")
    
    def broadcast(self, condition):
        """Broadcast warunku"""
        if not self.in_monitor:
            raise RuntimeError("Klient musi być w monitorze aby broadcastować")
        
        response = self._send_message({
            'type': 'BROADCAST_CONDITION',
            'condition': condition
        })
        
        if response['type'] == 'BROADCAST_CONFIRMED':
            woken = response['woken']
            self.logger.info(f"Broadcast '{condition}' - obudzono {woken} klientów")
        elif response['type'] == 'ERROR':
            error_msg = response['message']
            raise RuntimeError(f"Błąd broadcast: {error_msg}")
    
    def get_status(self):
        """Pobranie statusu serwera"""
        response = self._send_message({'type': 'STATUS'})
        
        if response['type'] == 'STATUS':
            return {
                'mutex_owner': response['mutex_owner'],
                'mutex_queue': response['mutex_queue'],
                'clients_in_monitor': response['clients_in_monitor'],
                'condition_queues': response['condition_queues']
            }
        else:
            return None
    
    def disconnect(self):
        """Rozłączenie z serwerem"""
        # Jeśli jesteśmy w monitorze, wyjdź z niego
        if self.in_monitor:
            try:
                self.exit()
            except:
                pass
        
        self._disconnect()
    
    def __enter__(self):
        """Context manager - wejście"""
        self.enter()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager - wyjście"""
        try:
            if self.in_monitor:
                self.exit()
        finally:
            self.disconnect()
    
    def __del__(self):
        """Destruktor"""
        self.disconnect()
        if self.context:
            self.context.term()

# Przykład użycia
if __name__ == "__main__":
    # Konfiguracja logowania
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    client = MonitorClient()
    
    try:
        # Test podstawowy
        print("🔐 Test podstawowego wejścia/wyjścia")
        client.enter()
        print("✅ Wszedłem do monitora")
        
        time.sleep(1)
        
        client.exit()
        print("✅ Wyszedłem z monitora")
        
        # Test statusu
        print("\n📊 Status serwera:")
        status = client.get_status()
        if status:
            print(f"Mutex owner: {status['mutex_owner']}")
            print(f"Queue length: {len(status['mutex_queue'])}")
        
    except Exception as e:
        print(f"❌ Błąd: {e}")
    finally:
        client.disconnect()