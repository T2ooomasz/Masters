#!/usr/bin/env python3
"""
REQ-REP Server - Zaawansowany wzorzec komunikacji ØMQ
Demonstracja przetwarzania różnych typów żądań z routing'iem

Ten serwer:
1. Przyjmuje różne typy żądań (PING, ECHO, STATUS, CALCULATE)
2. Routuje żądania do odpowiednich handler'ów
3. Symuluje różne czasy przetwarzania
4. Zarządza stanem serwera (statystyki, uptime)
5. Obsługuje błędy i nieprawidłowe żądania
"""

import zmq
import time
import json
import logging
from datetime import datetime, timedelta
from typing import Dict, Any, Optional

# Stałe do obsługi programu
PORT = 5555

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger("REQREPServer")

class REQREPServer:
    """
    Serwer REQ-REP z routing'iem żądań i zarządzaniem stanem
    
    Atrybuty:
        context: Kontekst ØMQ
        socket: Socket REP do komunikacji
        server_stats: Statystyki serwera (liczniki, czas startu)
        request_handlers: Mapa typów żądań do funkcji obsługujących
        is_running: Flaga kontrolująca główną pętlę serwera
    """
    
    def __init__(self, port: int = PORT):
        """
        Inicjalizacja serwera REQ-REP
        
        Args:
            port: Port na którym serwer nasłuchuje (domyślnie PORT)
        """
        # Konfiguracja ØMQ
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REP)
        self.address = f"tcp://*:{port}"
        
        # Stan serwera - statystyki i metryki
        self.server_stats = {
            'start_time': datetime.now(),
            'total_requests': 0,
            'successful_requests': 0,
            'failed_requests': 0,
            'request_types': {},  # Licznik dla każdego typu żądania
            'average_response_time': 0.0,
            'total_response_time': 0.0
        }
        
        # Routing żądań - mapa typu żądania do funkcji obsługującej
        self.request_handlers = {
            'PING': self._handle_ping,
            'ECHO': self._handle_echo,
            'STATUS': self._handle_status,
            'CALCULATE': self._handle_calculate,
            'TIMESTAMP': self._handle_timestamp,
            'UPTIME': self._handle_uptime
        }
        
        # Kontrola głównej pętli
        self.is_running = False
        
        logger.info(f"REQREPServer initialized on {self.address}")

    def start(self):
        """
        Uruchomienie serwera - bind socket i start głównej pętli
        """
        try:
            # Bind socket do adresu
            self.socket.bind(self.address)
            logger.info(f"Server bound to {self.address}")
            
            # Start głównej pętli obsługi żądań
            self.is_running = True
            logger.info("Server started, waiting for requests...")
            self._main_loop()
            
        except Exception as e:
            logger.error(f"Failed to start server: {e}")
            raise

    def _main_loop(self):
        """
        Główna pętla serwera - odbiera żądania i deleguje do handler'ów
        
        Proces obsługi każdego żądania:
        1. Odbierz żądanie (JSON string)
        2. Parse'uj JSON do dict
        3. Wyciągnij typ żądania
        4. Znajdź odpowiedni handler
        5. Wykonaj handler z mierzeniem czasu
        6. Wyślij odpowiedź
        7. Zaktualizuj statystyki
        """
        while self.is_running:
            try:
                # Krok 1: Odbierz żądanie od klienta
                start_time = time.time()
                raw_request = self.socket.recv_string()
                logger.info(f"Received raw request: {raw_request}")
                
                # Krok 2: Parse JSON request
                try:
                    request = json.loads(raw_request)
                except json.JSONDecodeError as e:
                    # Błędny JSON - wyślij błąd
                    error_response = self._create_error_response(
                        "INVALID_JSON", 
                        f"Cannot parse JSON: {e}"
                    )
                    self.socket.send_string(json.dumps(error_response))
                    self._update_stats('INVALID_JSON', time.time() - start_time, success=False)
                    continue
                
                # Krok 3: Wyciągnij typ żądania
                request_type = request.get('type', 'UNKNOWN')
                logger.info(f"Processing request type: {request_type}")
                
                # Krok 4: Znajdź handler dla typu żądania
                handler = self.request_handlers.get(request_type)
                
                if handler is None:
                    # Nieznany typ żądania
                    error_response = self._create_error_response(
                        "UNKNOWN_REQUEST_TYPE",
                        f"Unknown request type: {request_type}"
                    )
                    self.socket.send_string(json.dumps(error_response))
                    self._update_stats(request_type, time.time() - start_time, success=False)
                    continue
                
                # Krok 5: Wykonaj handler
                try:
                    response = handler(request)
                    response_time = time.time() - start_time
                    
                    # Krok 6: Wyślij odpowiedź
                    self.socket.send_string(json.dumps(response))
                    logger.info(f"Sent response for {request_type} in {response_time:.3f}s")
                    
                    # Krok 7: Zaktualizuj statystyki (sukces)
                    self._update_stats(request_type, response_time, success=True)
                    
                except Exception as e:
                    # Handler rzucił wyjątek
                    logger.error(f"Handler for {request_type} failed: {e}")
                    error_response = self._create_error_response(
                        "HANDLER_ERROR",
                        f"Handler failed: {str(e)}"
                    )
                    self.socket.send_string(json.dumps(error_response))
                    self._update_stats(request_type, time.time() - start_time, success=False)
                    
            except KeyboardInterrupt:
                # Ctrl+C - graceful shutdown
                logger.info("Received shutdown signal")
                break
            except Exception as e:
                logger.error(f"Unexpected error in main loop: {e}")
                # Spróbuj wyślać błąd jeśli socket jest dostępny
                try:
                    error_response = self._create_error_response(
                        "INTERNAL_ERROR",
                        "Internal server error"
                    )
                    self.socket.send_string(json.dumps(error_response))
                except:
                    pass

    def _handle_ping(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań PING - prosty test łączności
        
        Args:
            request: Żądanie w formacie {'type': 'PING', 'client_id': '...'}
            
        Returns:
            Dict z odpowiedzią PONG i timestamp
        """
        client_id = request.get('client_id', 'unknown')
        logger.info(f"PING from client: {client_id}")
        
        return {
            'status': 'success',
            'type': 'PONG',
            'timestamp': datetime.now().isoformat(),
            'client_id': client_id,
            'message': 'Server is alive'
        }

    def _handle_echo(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań ECHO - odbija wiadomość z powrotem
        
        Args:
            request: {'type': 'ECHO', 'message': 'tekst do odbicia', 'client_id': '...'}
            
        Returns:
            Dict z tym samym message ale z prefix'em
        """
        message = request.get('message', '')
        client_id = request.get('client_id', 'unknown')
        
        logger.info(f"ECHO from {client_id}: '{message}'")
        
        # Symulacja czasu przetwarzania
        time.sleep(0.1)
        
        return {
            'status': 'success',
            'type': 'ECHO_RESPONSE',
            'original_message': message,
            'echoed_message': f"ECHO: {message}",
            'client_id': client_id,
            'timestamp': datetime.now().isoformat()
        }

    def _handle_status(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań STATUS - zwraca statystyki serwera
        
        Args:
            request: {'type': 'STATUS', 'client_id': '...'}
            
        Returns:
            Dict z pełnymi statystykami serwera
        """
        client_id = request.get('client_id', 'unknown')
        logger.info(f"STATUS request from {client_id}")
        
        # Oblicz uptime
        uptime = datetime.now() - self.server_stats['start_time']
        
        return {
            'status': 'success',
            'type': 'STATUS_RESPONSE',
            'server_stats': {
                'uptime_seconds': uptime.total_seconds(),
                'uptime_formatted': str(uptime).split('.')[0],  # Bez mikrosekund
                'total_requests': self.server_stats['total_requests'],
                'successful_requests': self.server_stats['successful_requests'],
                'failed_requests': self.server_stats['failed_requests'],
                'success_rate': (self.server_stats['successful_requests'] / 
                               max(1, self.server_stats['total_requests']) * 100),
                'average_response_time_ms': self.server_stats['average_response_time'] * 1000,
                'request_types': self.server_stats['request_types'].copy()
            },
            'timestamp': datetime.now().isoformat()
        }

    def _handle_calculate(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań CALCULATE - wykonuje proste obliczenia
        
        Args:
            request: {'type': 'CALCULATE', 'operation': 'add', 'a': 5, 'b': 3}
            
        Returns:
            Dict z wynikiem obliczeń
        """
        client_id = request.get('client_id', 'unknown')
        operation = request.get('operation', '')
        a = request.get('a', 0)
        b = request.get('b', 0)
        
        logger.info(f"CALCULATE from {client_id}: {operation}({a}, {b})")
        
        calculation_time_start = time.time()
        try:
            if operation == 'add':
                result = a + b
            elif operation == 'subtract':
                result = a - b
            elif operation == 'multiply':
                result = a * b
            elif operation == 'divide':
                if b == 0:
                    raise ValueError("Division by zero")
                result = a / b
            elif operation == 'power':
                result = a ** b
            else:
                raise ValueError(f"Unknown operation: {operation}")
                
            return {
                'status': 'success',
                'type': 'CALCULATE_RESPONSE',
                'operation': operation,
                'operands': {'a': a, 'b': b},
                'result': result,
                'calculation_time_ms': calculation_time_start - time.time(),
                'timestamp': datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"Calculation error: {e}")
            return {
                'status': 'error',
                'type': 'CALCULATE_ERROR',
                'error': str(e),
                'operation': operation,
                'operands': {'a': a, 'b': b}
            }

    def _handle_timestamp(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań TIMESTAMP - zwraca aktualny czas
        """
        client_id = request.get('client_id', 'unknown')
        now = datetime.now()
        
        return {
            'status': 'success',
            'type': 'TIMESTAMP_RESPONSE',
            'timestamp': now.isoformat(),
            'unix_timestamp': now.timestamp(),
            'formatted_time': now.strftime('%Y-%m-%d %H:%M:%S'),
            'client_id': client_id
        }

    def _handle_uptime(self, request: Dict[str, Any]) -> Dict[str, Any]:
        """
        Handler dla żądań UPTIME - zwraca czas działania serwera
        """
        uptime = datetime.now() - self.server_stats['start_time']
        
        return {
            'status': 'success',
            'type': 'UPTIME_RESPONSE',
            'uptime_seconds': uptime.total_seconds(),
            'uptime_formatted': str(uptime).split('.')[0],
            'start_time': self.server_stats['start_time'].isoformat()
        }

    def _create_error_response(self, error_code: str, error_message: str) -> Dict[str, Any]:
        """
        Tworzy standardową odpowiedź błędu
        
        Args:
            error_code: Kod błędu (np. 'INVALID_JSON', 'UNKNOWN_REQUEST_TYPE')
            error_message: Opis błędu
            
        Returns:
            Dict z odpowiedzią błędu
        """
        return {
            'status': 'error',
            'error_code': error_code,
            'error_message': error_message,
            'timestamp': datetime.now().isoformat()
        }

    def _update_stats(self, request_type: str, response_time: float, success: bool):
        """
        Aktualizuje statystyki serwera po obsłużeniu żądania
        
        Args:
            request_type: Typ żądania (PING, ECHO, etc.)
            response_time: Czas obsługi w sekundach
            success: Czy żądanie zostało obsłużone pomyślnie
        """
        # Aktualizuj podstawowe liczniki
        self.server_stats['total_requests'] += 1
        
        if success:
            self.server_stats['successful_requests'] += 1
        else:
            self.server_stats['failed_requests'] += 1
        
        # Aktualizuj licznik dla typu żądania
        if request_type not in self.server_stats['request_types']:
            self.server_stats['request_types'][request_type] = 0
        self.server_stats['request_types'][request_type] += 1
        
        # Aktualizuj średni czas odpowiedzi (tylko dla udanych żądań)
        if success:
            total_time = self.server_stats['total_response_time']
            total_successful = self.server_stats['successful_requests']
            
            # Oblicz nową średnią: (stara_suma + nowy_czas) / nowa_liczba
            self.server_stats['total_response_time'] = total_time + response_time
            self.server_stats['average_response_time'] = (
                self.server_stats['total_response_time'] / total_successful
            )

    def stop(self):
        """
        Zatrzymuje serwer - ustawia flagę is_running na False
        """
        logger.info("Stopping server...")
        self.is_running = False

    def cleanup(self):
        """
        Czyści zasoby - zamyka socket i context
        """
        logger.info("Cleaning up server resources...")
        if hasattr(self, 'socket'):
            self.socket.close()
        if hasattr(self, 'context'):
            self.context.term()
        logger.info("Server cleanup completed")

def main():
    """
    Główna funkcja - tworzy i uruchamia serwer
    """
    server = None
    try:
        # Tworzenie i uruchomienie serwera
        server = REQREPServer(port=PORT)
        server.start()
        
    except KeyboardInterrupt:
        logger.info("Received Ctrl+C, shutting down...")
    except Exception as e:
        logger.error(f"Server error: {e}")
    finally:
        # Zawsze wykonaj cleanup
        if server:
            server.stop()
            server.cleanup()

if __name__ == "__main__":
    main()