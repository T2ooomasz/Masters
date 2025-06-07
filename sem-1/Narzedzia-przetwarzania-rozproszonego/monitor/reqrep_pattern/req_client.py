#!/usr/bin/env python3
"""
REQ-REP Client - Zaawansowany klient ØMQ z obsługą różnych typów żądań
Demonstracja pełnego API klient-serwer z JSON messaging

Ten klient:
1. Łączy się z serwerem REQ-REP
2. Wysyła różne typy żądań (PING, ECHO, STATUS, CALCULATE)
3. Mierzy czasy odpowiedzi
4. Obsługuje błędy i timeout'y
5. Oferuje tryb interaktywny i automatyczny
"""

import zmq
import json
import time
import logging
import argparse
from datetime import datetime
from typing import Dict, Any, Optional, List

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)
logger = logging.getLogger("REQREPClient")

class REQREPClient:
    """
    Klient REQ-REP z obsługą różnych typów żądań i timeout'ów
    
    Atrybuty:
        context: Kontekst ØMQ
        socket: Socket REQ do komunikacji
        client_id: Unikalny identyfikator klienta
        server_address: Adres serwera
        timeout_ms: Timeout dla żądań w milisekundach
        request_stats: Statystyki wysłanych żądań
    """
    
    def __init__(self, server_address: str = "tcp://localhost:5555", timeout_ms: int = 5000):
        """
        Inicjalizacja klienta REQ-REP
        
        Args:
            server_address: Adres serwera (domyślnie localhost:5555)
            timeout_ms: Timeout dla żądań w ms (domyślnie 5000ms = 5s)
        """
        # Konfiguracja ØMQ
        self.context = zmq.Context()
        self.socket = self.context.socket(zmq.REQ)
        self.server_address = server_address
        self.timeout_ms = timeout_ms
        
        # Unikalny identyfikator klienta
        self.client_id = f"Client-{datetime.now().strftime('%H%M%S')}-{id(self) % 1000}"
        
        # Statystyki klienta
        self.request_stats = {
            'total_requests': 0,
            'successful_requests': 0,
            'failed_requests': 0,
            'timeouts': 0,
            'total_response_time': 0.0,
            'min_response_time': float('inf'),
            'max_response_time': 0.0,
            'request_types': {}
        }
        
        logger.info(f"REQREPClient '{self.client_id}' initialized")

    def connect(self) -> bool:
        """
        Łączy się z serwerem
        
        Returns:
            True jeśli połączenie udane, False w przeciwnym razie
        """
        try:
            # Ustaw timeout na socket
            self.socket.setsockopt(zmq.RCVTIMEO, self.timeout_ms)
            self.socket.setsockopt(zmq.SNDTIMEO, self.timeout_ms)
            
            # Połącz z serwerem
            self.socket.connect(self.server_address)
            logger.info(f"Connected to server at {self.server_address}")
            
            # Test połączenia z PING
            test_response = self.ping()
            if test_response and test_response.get('status') == 'success':
                logger.info("Connection test successful")
                return True
            else:
                logger.error("Connection test failed")
                return False
                
        except Exception as e:
            logger.error(f"Failed to connect to server: {e}")
            return False

    def _send_request(self, request: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie do serwera i zwraca odpowiedź
        
        Args:
            request: Żądanie w formacie dict
            
        Returns:
            Odpowiedź od serwera lub None w przypadku błędu/timeout'u
        """
        request_type = request.get('type', 'UNKNOWN')
        start_time = time.time()
        
        try:
            # Dodaj client_id do żądania
            request['client_id'] = self.client_id
            
            # Serializuj do JSON i wyślij
            request_json = json.dumps(request)
            logger.info(f"Sending {request_type} request")
            logger.debug(f"Request JSON: {request_json}")
            
            self.socket.send_string(request_json)
            
            # Odbierz odpowiedź
            response_json = self.socket.recv_string()
            response_time = time.time() - start_time
            
            logger.info(f"Received response for {request_type} in {response_time:.3f}s")
            logger.debug(f"Response JSON: {response_json}")
            
            # Parse odpowiedzi
            response = json.loads(response_json)
            
            # Aktualizuj statystyki
            self._update_stats(request_type, response_time, True)
            
            return response
            
        except zmq.Again:
            # Timeout
            response_time = time.time() - start_time
            logger.error(f"Timeout for {request_type} request after {response_time:.3f}s")
            self._update_stats(request_type, response_time, False, timeout=True)
            return None
            
        except json.JSONDecodeError as e:
            response_time = time.time() - start_time
            logger.error(f"Failed to parse response JSON: {e}")
            self._update_stats(request_type, response_time, False)
            return None
            
        except Exception as e:
            response_time = time.time() - start_time
            logger.error(f"Error sending {request_type} request: {e}")
            self._update_stats(request_type, response_time, False)
            return None

    def ping(self) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie PING - test łączności
        
        Returns:
            Odpowiedź PONG od serwera
        """
        request = {
            'type': 'PING'
        }
        return self._send_request(request)

    def echo(self, message: str) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie ECHO - odbicia wiadomości
        
        Args:
            message: Wiadomość do odbicia
            
        Returns:
            Odpowiedź z odbicia wiadomości
        """
        request = {
            'type': 'ECHO',
            'message': message
        }
        return self._send_request(request)

    def get_status(self) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie STATUS - statystyki serwera
        
        Returns:
            Odpowiedź ze statystykami serwera
        """
        request = {
            'type': 'STATUS'
        }
        return self._send_request(request)

    def calculate(self, operation: str, a: float, b: float) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie CALCULATE - obliczenia matematyczne
        
        Args:
            operation: Operacja (add, subtract, multiply, divide, power)
            a: Pierwszy operand
            b: Drugi operand
            
        Returns:
            Odpowiedź z wynikiem obliczeń
        """
        request = {
            'type': 'CALCULATE',
            'operation': operation,
            'a': a,
            'b': b
        }
        return self._send_request(request)

    def get_timestamp(self) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie TIMESTAMP - aktualny czas serwera
        
        Returns:
            Odpowiedź z timestamp'em serwera
        """
        request = {
            'type': 'TIMESTAMP'
        }
        return self._send_request(request)

    def get_uptime(self) -> Optional[Dict[str, Any]]:
        """
        Wysyła żądanie UPTIME - czas działania serwera
        
        Returns:
            Odpowiedź z uptime serwera
        """
        request = {
            'type': 'UPTIME'
        }
        return self._send_request(request)

    def _update_stats(self, request_type: str, response_time: float, success: bool, timeout: bool = False):
        """
        Aktualizuje statystyki klienta
        
        Args:
            request_type: Typ żądania
            response_time: Czas odpowiedzi w sekundach
            success: Czy żądanie zakończone sukcesem
            timeout: Czy wystąpił timeout
        """
        # Podstawowe liczniki
        self.request_stats['total_requests'] += 1
        
        if success:
            self.request_stats['successful_requests'] += 1
            
            # Czasy odpowiedzi (tylko dla udanych żądań)
            self.request_stats['total_response_time'] += response_time
            self.request_stats['min_response_time'] = min(
                self.request_stats['min_response_time'], response_time
            )
            self.request_stats['max_response_time'] = max(
                self.request_stats['max_response_time'], response_time
            )
        else:
            self.request_stats['failed_requests'] += 1
            if timeout:
                self.request_stats['timeouts'] += 1
        
        # Licznik dla typu żądania
        if request_type not in self.request_stats['request_types']:
            self.request_stats['request_types'][request_type] = {'total': 0, 'success': 0, 'failed': 0}
        
        self.request_stats['request_types'][request_type]['total'] += 1
        if success:
            self.request_stats['request_types'][request_type]['success'] += 1
        else:
            self.request_stats['request_types'][request_type]['failed'] += 1

    def print_stats(self):
        """
        Wyświetla statystyki klienta
        """
        print("\n" + "="*50)
        print(f"CLIENT STATISTICS - {self.client_id}")
        print("="*50)
        
        total = self.request_stats['total_requests']
        successful = self.request_stats['successful_requests']
        
        print(f"Total requests: {total}")
        print(f"Successful: {successful}")
        print(f"Failed: {self.request_stats['failed_requests']}")
        print(f"Timeouts: {self.request_stats['timeouts']}")
        
        if total > 0:
            success_rate = (successful / total) * 100
            print(f"Success rate: {success_rate:.1f}%")
        
        if successful > 0:
            avg_time = self.request_stats['total_response_time'] / successful
            min_time = self.request_stats['min_response_time']
            max_time = self.request_stats['max_response_time']
            
            print(f"Average response time: {avg_time:.3f}s")
            print(f"Min response time: {min_time:.3f}s")
            print(f"Max response time: {max_time:.3f}s")
        
        print("\nRequest types:")
        for req_type, stats in self.request_stats['request_types'].items():
            print(f"  {req_type}: {stats['success']}/{stats['total']} successful")

    def run_interactive_mode(self):
        """
        Uruchamia tryb interaktywny - użytkownik wprowadza komendy
        """
        print(f"\n=== Interactive Mode - Client {self.client_id} ===")
        print("Available commands:")
        print("  ping - Test connection")
        print("  echo <message> - Echo message")
        print("  status - Get server status")
        print("  calc <op> <a> <b> - Calculate (add/sub/mul/div/pow)")
        print("  time - Get server timestamp")
        print("  uptime - Get server uptime")
        print("  stats - Show client statistics")
        print("  quit - Exit")
        print()
        
        while True:
            try:
                command = input(f"[{self.client_id}]> ").strip().split()
                if not command:
                    continue
                    
                cmd = command[0].lower()
                
                if cmd == 'quit':
                    break
                elif cmd == 'ping':
                    response = self.ping()
                    if response:
                        print(f"PONG: {response.get('message', 'No message')}")
                    else:
                        print("PING failed")
                        
                elif cmd == 'echo':
                    if len(command) < 2:
                        print("Usage: echo <message>")
                        continue
                    message = ' '.join(command[1:])
                    response = self.echo(message)
                    if response:
                        print(f"Echo: {response.get('echoed_message', 'No echo')}")
                    else:
                        print("Echo failed")
                        
                elif cmd == 'status':
                    response = self.get_status()
                    if response and 'server_stats' in response:
                        stats = response['server_stats']
                        print(f"Server uptime: {stats.get('uptime_formatted', 'Unknown')}")
                        print(f"Total requests: {stats.get('total_requests', 0)}")
                        print(f"Success rate: {stats.get('success_rate', 0):.1f}%")
                        print(f"Avg response time: {stats.get('average_response_time_ms', 0):.1f}ms")
                    else:
                        print("Status request failed")
                        
                elif cmd == 'calc':
                    if len(command) < 4:
                        print("Usage: calc <operation> <a> <b>")
                        print("Operations: add, subtract, multiply, divide, power")
                        continue
                    try:
                        op = command[1]
                        a = float(command[2])
                        b = float(command[3])
                        response = self.calculate(op, a, b)
                        if response and response.get('status') == 'success':
                            result = response.get('result')
                            print(f"{op}({a}, {b}) = {result}")
                        else:
                            error = response.get('error', 'Unknown error') if response else 'Request failed'
                            print(f"Calculation failed: {error}")
                    except ValueError:
                        print("Invalid numbers")
                        
                elif cmd == 'time':
                    response = self.get_timestamp()
                    if response:
                        print(f"Server time: {response.get('formatted_time', 'Unknown')}")
                    else:
                        print("Time request failed")
                        
                elif cmd == 'uptime':
                    response = self.get_uptime()
                    if response:
                        print(f"Server uptime: {response.get('uptime_formatted', 'Unknown')}")
                    else:
                        print("Uptime request failed")
                        
                elif cmd == 'stats':
                    self.print_stats()
                    
                else:
                    print(f"Unknown command: {cmd}")
                    
            except KeyboardInterrupt:
                print("\nExiting...")
                break
            except Exception as e:
                print(f"Error: {e}")

    def run_automatic_demo(self):
        """
        Uruchamia automatyczną demonstrację - wykonuje serię żądań
        """
        print(f"\n=== Automatic Demo - Client {self.client_id} ===")
        
        # Lista żądań do wykonania
        demo_requests = [
            ("PING", lambda: self.ping()),
            ("ECHO 'Hello World'", lambda: self.echo("Hello World")),
            ("CALCULATE add(10, 5)", lambda: self.calculate("add", 10, 5)),
            ("CALCULATE multiply(7, 8)", lambda: self.calculate("multiply", 7, 8)),
            ("CALCULATE divide(20, 4)", lambda: self.calculate("divide", 20, 4)),
            ("GET TIMESTAMP", lambda: self.get_timestamp()),
            ("GET UPTIME", lambda: self.get_uptime()),
            ("GET STATUS", lambda: self.get_status()),
            ("ECHO 'Final test'", lambda: self.echo("Final test"))
        ]
        
        for description, request_func in demo_requests:
            print(f"\n>>> {description}")
            response = request_func()
            
            if response:
                status = response.get('status', 'unknown')
                if status == 'success':
                    print(f"✓ SUCCESS")
                    # Wyświetl specyficzne informacje zależnie od typu
                    response_type = response.get('type', '')
                    if response_type == 'PONG':
                        print(f"  Message: {response.get('message')}")
                    elif response_type == 'ECHO_RESPONSE':
                        print(f"  Echo: {response.get('echoed_message')}")
                    elif response_type == 'CALCULATE_RESPONSE':
                        print(f"  Result: {response.get('result')}")
                    elif response_type == 'TIMESTAMP_RESPONSE':
                        print(f"  Time: {response.get('formatted_time')}")
                    elif response_type == 'UPTIME_RESPONSE':
                        print(f"  Uptime: {response.get('uptime_formatted')}")
                    elif response_type == 'STATUS_RESPONSE':
                        stats = response.get('server_stats', {})
                        print(f"  Server requests: {stats.get('total_requests', 0)}")
                        print(f"  Success rate: {stats.get('success_rate', 0):.1f}%")
                else:
                    print(f"✗ ERROR: {response.get('error_message', 'Unknown error')}")
            else:
                print("✗ REQUEST FAILED (timeout or error)")
            
            # Pauza między żądaniami
            time.sleep(0.5)
        
        print(f"\n=== Demo completed ===")
        self.print_stats()

    def cleanup(self):
        """
        Czyści zasoby klienta
        """
        logger.info("Cleaning up client resources...")
        if hasattr(self, 'socket'):
            self.socket.close()
        if hasattr(self, 'context'):
            self.context.term()
        logger.info("Client cleanup completed")

def main():
    """
    Główna funkcja - parsuje argumenty i uruchamia klienta
    """
    parser = argparse.ArgumentParser(description='REQ-REP Client')
    parser.add_argument('--server', default='tcp://localhost:5555', help='Server address')
    parser.add_argument('--timeout', type=int, default=5000, help='Timeout in milliseconds')
    parser.add_argument('--interactive', '-i', action='store_true', help='Interactive mode')
    parser.add_argument('--demo', '-d', action='store_true', help='Automatic demo mode')
    
    args = parser.parse_args()
    
    client = None
    try:
        # Tworzenie klienta
        client = REQREPClient(args.server, args.timeout)
        
        # Połączenie z serwerem
        if not client.connect():
            print("Failed to connect to server")
            return
        
        # Wybór trybu działania
        if args.interactive:
            client.run_interactive_mode()
        elif args.demo:
            client.run_automatic_demo()
        else:
            # Domyślnie uruchom demo
            print("No mode specified, running automatic demo...")
            client.run_automatic_demo()
            
    except KeyboardInterrupt:
        logger.info("Received Ctrl+C, shutting down client...")
    except Exception as e:
        logger.error(f"Client error: {e}")
    finally:
        # Zawsze wykonaj cleanup
        if client:
            client.cleanup()

if __name__ == "__main__":
    main()