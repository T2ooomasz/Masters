"""
Biblioteka kliencka dla rozproszonego monitora z ØMQ
Zapewnia transparentny interfejs podobny do standardowych monitorów

Protokół komunikacyjny:
1. ENTER_MONITOR -> {status: "granted"/"queued"}
2. Jeśli "queued", potem CHECK_GRANT -> {status: "granted"/"queued"}
3. WAIT -> {status: "waiting_on_condition"}
4. Potem CHECK_AWAKENED -> {status: "granted_after_wait"/"still_waiting_on_condition"/"queued_after_signal"}
5. SIGNAL/BROADCAST -> {status: "success", woken_processes: int}
6. EXIT_MONITOR -> {status: "success"}

Polling strategy pozwala na pełne blokowanie operacji enter() i wait()
bez ciągłego wysyłania tych samych żądań do serwera.
"""

import zmq
import json
import uuid
import time
import threading
from typing import Optional, Dict, Any


class MonitorError(Exception):
    """Wyjątek dla błędów monitora"""
    pass


class MonitorTimeoutError(MonitorError):
    """Wyjątek dla timeoutów operacji monitora"""
    pass


class DistributedMonitor:
    """
    Klient rozproszonego monitora zapewniający transparentny interfejs
    
    Używa wzorca REQ-REP do komunikacji z centralnym serwerem monitora.
    Zapewnia operacje mutual exclusion oraz condition variables.
    
    Przykład użycia:
        monitor = DistributedMonitor("buffer_monitor", "tcp://localhost:5555")
        
        # Klasyczne użycie
        monitor.enter()
        try:
            while not condition_met:
                monitor.wait("condition_name")
            # sekcja krytyczna
            monitor.signal("other_condition")
        finally:
            monitor.exit()
            
        # Lub z context manager
        with monitor:
            while not condition_met:
                monitor.wait("condition_name")
            monitor.signal("other_condition")
    """
    
    def __init__(self, monitor_name: str, server_address: str, timeout: int = 30000, poll_interval: float = 0.1):
        """
        Inicjalizacja klienta monitora
        
        Args:
            monitor_name: nazwa monitora (wiele procesów może używać tej samej nazwy)
            server_address: adres serwera (np. "tcp://localhost:5555")
            timeout: timeout operacji w ms (domyślnie 30s)
            poll_interval: interwał sprawdzania statusu w sekundach (domyślnie 0.1s)
        """
        self.monitor_name = monitor_name
        self.server_address = server_address
        self.timeout = timeout
        self.poll_interval = poll_interval
        self.process_id = f"{uuid.uuid4()}-{threading.get_ident()}"
        
        # Stan klienta
        self._entered = False
        self._socket_lock = threading.Lock()  # Ochrona przed współbieżnym dostępem do socketu
        self._current_condition: Optional[str] = None  # Warunek na którym obecnie czekamy
        
        # Inicjalizacja ØMQ
        self._context = zmq.Context()
        self._socket: Optional[zmq.Socket] = None
        self._connect()
    
    def _connect(self) -> None:
        """Nawiązanie połączenia z serwerem monitora"""
        try:
            if self._socket:
                self._socket.close()
            
            self._socket = self._context.socket(zmq.REQ)
            self._socket.setsockopt(zmq.RCVTIMEO, self.timeout)
            self._socket.setsockopt(zmq.SNDTIMEO, self.timeout)
            self._socket.setsockopt(zmq.LINGER, 1000)  # Czas oczekiwania na zamknięcie
            self._socket.connect(self.server_address)
            
        except zmq.ZMQError as e:
            raise MonitorError(f"Nie można połączyć z serwerem monitora: {e}")
    
    def _send_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Wysłanie żądania do serwera i otrzymanie odpowiedzi
        
        Args:
            request: słownik z żądaniem
            
        Returns:
            słownik z odpowiedzią serwera
            
        Raises:
            MonitorTimeoutError: przy timeout
            MonitorError: przy innych błędach komunikacji
        """
        with self._socket_lock:
            try:
                # Dodanie metadanych do żądania
                request.update({
                    "process_id": self.process_id,
                    "monitor_name": self.monitor_name
                })
                
                # Wysłanie żądania
                message = json.dumps(request).encode('utf-8')
                self._socket.send(message)
                
                # Oczekiwanie na odpowiedź
                response_bytes = self._socket.recv()
                response = json.loads(response_bytes.decode('utf-8'))
                
                return response
                
            except zmq.Again:
                # Timeout - próba ponownego połączenia
                self._connect()
                raise MonitorTimeoutError(f"Timeout operacji monitora po {self.timeout}ms")
                
            except zmq.ZMQError as e:
                # Błąd komunikacji - próba ponownego połączenia
                self._connect()
                raise MonitorError(f"Błąd komunikacji z serwerem: {e}")
                
            except (json.JSONDecodeError, UnicodeDecodeError) as e:
                raise MonitorError(f"Błąd dekodowania odpowiedzi serwera: {e}")
    
    def enter(self) -> None:
        """
        Wejście do monitora (acquire mutex)
        Blokuje aż do uzyskania dostępu
        
        Raises:
            MonitorError: jeśli już jesteśmy w monitorze
            MonitorTimeoutError: przy timeout
        """
        if self._entered:
            raise MonitorError("Proces już znajduje się w monitorze")
        
        # Wysłanie pierwotnego żądania wejścia
        request = {
            "action": "ENTER"
        }
        
        response = self._send_request(request)
        
        if response.get("status") == "granted":
            self._entered = True
            return
        elif response.get("status") == "queued":
            # Zostaliśmy dodani do kolejki, teraz sprawdzamy periodycznie
            self._wait_for_grant()
        elif response.get("status") == "error":
            raise MonitorError(f"Błąd wejścia do monitora: {response.get('message', 'Nieznany błąd')}")
        else:
            raise MonitorError(f"Nieoczekiwana odpowiedź serwera: {response}")
    
    def _wait_for_grant(self) -> None:
        """
        Oczekiwanie na przyznanie dostępu do monitora przez polling
        """
        check_request = {
            "action": "CHECK_GRANT"
        }
        
        while True:
            time.sleep(self.poll_interval)
            response = self._send_request(check_request)
            
            if response.get("status") == "granted":
                self._entered = True
                return
            elif response.get("status") == "queued":
                continue  # Nadal czekamy w kolejce
            elif response.get("status") == "error":
                raise MonitorError(f"Błąd podczas oczekiwania na dostęp: {response.get('message', 'Nieznany błąd')}")
            else:
                raise MonitorError(f"Nieoczekiwana odpowiedź serwera: {response}")
    
    def exit(self) -> None:
        """
        Wyjście z monitora (release mutex)
        
        Raises:
            MonitorError: jeśli nie jesteśmy w monitorze
        """
        if not self._entered:
            raise MonitorError("Proces nie znajduje się w monitorze")
        
        request = {
            "action": "EXIT"
        }
        
        response = self._send_request(request)
        
        if response.get("status") == "success":
            self._entered = False
        else:
            raise MonitorError(f"Błąd wyjścia z monitora: {response.get('message', 'Nieznany błąd')}")
    
    def wait(self, condition_name: str) -> None:
        """
        Oczekiwanie na warunek - zwalnia mutex i czeka na signal/broadcast
        Po otrzymaniu sygnału ponownie uzyskuje mutex
        
        Args:
            condition_name: nazwa warunku do oczekiwania
            
        Raises:
            MonitorError: jeśli nie jesteśmy w monitorze
            MonitorTimeoutError: przy timeout
        """
        if not self._entered:
            raise MonitorError("Proces musi być w monitorze aby wywołać wait()")
        
        if not condition_name:
            raise MonitorError("Nazwa warunku nie może być pusta")
        
        self._entered = False # Zwalniamy mutex przed oczekiwaniem
        self._current_condition = condition_name
        
        # Wysłanie żądania wait - mutex zostanie zwolniony, proces dodany do kolejki warunku
        request = {
            "action": "WAIT",
            "condition_name": condition_name
        }
        
        try:
            response = self._send_request(request)
            
            if response.get("status") == "waiting_on_condition":
                # Zostaliśmy dodani do kolejki warunku, mutex został zwolniony
                # Teraz sprawdzamy periodycznie czy zostaliśmy wybudzeni i odzyskaliśmy mutex
                self._wait_for_awakening(condition_name)
            elif response.get("status") == "error":
                self._entered = True # Błąd, więc nie weszliśmy w stan wait, przywracamy _entered
                self._current_condition = None
                raise MonitorError(f"Błąd wait(): {response.get('message', 'Nieznany błąd')}")
            else:
                self._entered = True # Nieznana odpowiedź, przywracamy _entered
                self._current_condition = None
                raise MonitorError(f"Nieoczekiwana odpowiedź serwera na WAIT: {response}")
        except Exception as e:
            self._entered = True # Na wypadek błędu, przywracamy _entered
            self._current_condition = None
            raise e
    
    def _wait_for_awakening(self, condition_name: str) -> None:
        """
        Oczekiwanie na wybudzenie z warunku przez polling
        
        Args:
            condition_name: nazwa warunku na którym czekamy
        """
        check_request = {
            "action": "CHECK_AWAKENED",
            "condition_name": condition_name
        }
        
        while True:
            time.sleep(self.poll_interval)
            response = self._send_request(check_request)
            
            status = response.get("status")
            if status == "granted_after_wait":
                # Zostaliśmy wybudzeni i dostaliśmy z powrotem mutex
                self._entered = True
                self._current_condition = None
                return
            elif status in ["still_waiting_on_condition", "queued_after_signal"]:
                continue  # Nadal czekamy na warunek lub na mutex po sygnale
            elif status == "error":
                raise MonitorError(f"Błąd podczas oczekiwania na wybudzenie: {response.get('message', 'Nieznany błąd')}")
            else:
                raise MonitorError(f"Nieoczekiwana odpowiedź serwera: {response}")
    
    def signal(self, condition_name: str) -> int:
        """
        Sygnalizacja warunku - wybudza jeden oczekujący proces
        
        Args:
            condition_name: nazwa warunku do zasygnalizowania
            
        Returns:
            liczba wybudzonych procesów (0 lub 1)
            
        Raises:
            MonitorError: jeśli nie jesteśmy w monitorze
        """
        if not self._entered:
            raise MonitorError("Proces musi być w monitorze aby wywołać signal()")
        
        if not condition_name:
            raise MonitorError("Nazwa warunku nie może być pusta")
        
        request = {
            "action": "SIGNAL",
            "condition_name": condition_name
        }
        
        response = self._send_request(request)
        
        if response.get("status") == "success":
            return response.get("woken_processes", 0)
        else:
            raise MonitorError(f"Błąd signal(): {response.get('message', 'Nieznany błąd')}")
    
    def broadcast(self, condition_name: str) -> int:
        """
        Broadcast warunku - wybudza wszystkie oczekujące procesy
        
        Args:
            condition_name: nazwa warunku do rozgłoszenia
            
        Returns:
            liczba wybudzonych procesów
            
        Raises:
            MonitorError: jeśli nie jesteśmy w monitorze
        """
        if not self._entered:
            raise MonitorError("Proces musi być w monitorze aby wywołać broadcast()")
        
        if not condition_name:
            raise MonitorError("Nazwa warunku nie może być pusta")
        
        request = {
            "action": "BROADCAST",
            "condition_name": condition_name
        }
        
        response = self._send_request(request)
        
        if response.get("status") == "success":
            return response.get("woken_processes", 0)
        else:
            raise MonitorError(f"Błąd broadcast(): {response.get('message', 'Nieznany błąd')}")
    
    def is_entered(self) -> bool:
        """
        Sprawdzenie czy proces znajduje się w monitorze
        
        Returns:
            True jeśli proces jest w monitorze
        """
        return self._entered
    
    def get_current_condition(self) -> Optional[str]:
        """
        Pobranie nazwy warunku na którym obecnie czekamy
        
        Returns:
            nazwa warunku lub None gdy nie czekamy na żaden warunek
        """
        return self._current_condition
    
    def close(self) -> None:
        """
        Zamknięcie połączenia z serwerem
        Automatycznie wywołuje exit() jeśli jesteśmy w monitorze
        """
        try:
            if self._entered:
                self.exit()
        except MonitorError:
            pass  # Ignorujemy błędy przy zamykaniu
        
        if self._socket:
            self._socket.close()
        
        if self._context:
            self._context.term()
    
    def __enter__(self):
        """Wsparcie dla 'with' statement - automatyczne wejście do monitora"""
        self.enter()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Automatyczne wyjście z monitora przy opuszczeniu bloku 'with'"""
        if self._entered:
            self.exit()
        return False  # Nie przechwytujemy wyjątków
    
    def __del__(self):
        """Destruktor - zapewnia zamknięcie połączenia"""
        try:
            self.close()
        except:
            pass  # Ignorujemy błędy przy usuwaniu obiektu


class MonitorPool:
    """
    Pula połączeń do monitorów dla optymalizacji wydajności
    Umożliwia współdzielenie połączeń między wątkami tego samego procesu
    """
    
    def __init__(self, server_address: str, timeout: int = 30000):
        """
        Args:
            server_address: adres serwera monitora
            timeout: timeout operacji w ms
        """
        self.server_address = server_address
        self.timeout = timeout
        self._monitors: Dict[str, DistributedMonitor] = {}
        self._lock = threading.Lock()
    
    def get_monitor(self, monitor_name: str) -> DistributedMonitor:
        """
        Pobranie monitora z puli (utworzenie jeśli nie istnieje)
        
        Args:
            monitor_name: nazwa monitora
            
        Returns:
            instancja DistributedMonitor
        """
        with self._lock:
            if monitor_name not in self._monitors:
                self._monitors[monitor_name] = DistributedMonitor(
                    monitor_name, self.server_address, self.timeout
                )
            return self._monitors[monitor_name]
    
    def close_all(self) -> None:
        """Zamknięcie wszystkich połączeń w puli"""
        with self._lock:
            for monitor in self._monitors.values():
                monitor.close()
            self._monitors.clear()


# Przykład użycia
if __name__ == "__main__":
    # Przykład prostego użycia z nowym protokołem
    try:
        # Konfigurowalny interwał pollingu dla lepszej wydajności
        monitor = DistributedMonitor("test_monitor", "tcp://localhost:5555", poll_interval=0.05)
        
        print("Próbuję wejść do monitora...")
        monitor.enter()  # Może być queued, potem polling CHECK_GRANT
        print("Jestem w monitorze!")
        
        print("Symulacja wait() na warunku...")
        # W prawdziwym scenariuszu wait() byłby wywoływany gdy warunek nie jest spełniony
        # monitor.wait("some_condition")  # -> waiting_on_condition, potem polling CHECK_AWAKENED
        
        print("Wykonuję operacje w sekcji krytycznej...")
        time.sleep(1)
        
        print("Wychodzę z monitora...")
        monitor.exit()
        
        print("Test z context manager...")
        with monitor:
            print("W monitorze przez context manager")
            print(f"Status: entered={monitor.is_entered()}, condition={monitor.get_current_condition()}")
            time.sleep(0.5)
        
        monitor.close()
        print("Test zakończony pomyślnie!")
        
    except MonitorError as e:
        print(f"Błąd monitora: {e}")
    except Exception as e:
        print(f"Nieoczekiwany błąd: {e}")