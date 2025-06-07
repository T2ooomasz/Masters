#!/usr/bin/env python3
"""
Hello Client - Podstawowy klient ØMQ
Demonstracja wzorca REQ-REP (Request-Reply)

Ten klient:
1. Łączy się z serwerem na localhost:5555
2. Wysyła serię wiadomości
3. Czeka na odpowiedzi od serwera
4. Loguje wszystkie operacje
"""

import zmq
import time
import logging
import sys
from datetime import datetime

# Konfiguracja logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)

def send_message(socket, message, client_id):
    """
    Funkcja wysyłająca pojedynczą wiadomość do serwera
    
    Args:
        socket: Socket ØMQ typu REQ
        message: Wiadomość do wysłania (string)
        client_id: Identyfikator klienta (string)
    
    Returns:
        str: Odpowiedź od serwera lub None w przypadku błędu
    """
    logger = logging.getLogger(f"Client-{client_id}")
    
    try:
        # 1. PRZYGOTOWANIE WIADOMOŚCI
        # Dodajemy identyfikator klienta do wiadomości
        full_message = f"[{client_id}] {message}"
        
        # 2. WYSYŁANIE WIADOMOŚCI
        logger.info(f"Wysyłanie: '{full_message}'")
        socket.send_string(full_message)
        
        # 3. OCZEKIWANIE NA ODPOWIEDŹ
        # recv_string() blokuje do momentu otrzymania odpowiedzi
        # W wzorcu REQ-REP musimy zawsze czekać na odpowiedź
        logger.info("Oczekiwanie na odpowiedź...")
        response = socket.recv_string()
        
        # 4. LOGOWANIE ODPOWIEDZI
        logger.info(f"Otrzymano odpowiedź: '{response}'")
        
        return response
        
    except zmq.ZMQError as e:
        logger.error(f"Błąd ØMQ: {e}")
        return None
    except Exception as e:
        logger.error(f"Wystąpił błąd: {e}")
        return None

def main():
    """
    Główna funkcja klienta
    """
    logger = logging.getLogger("HelloClient")
    logger.info("Uruchamianie Hello Client...")
    
    # 1. PARAMETRY KLIENTA
    # Identyfikator klienta dla rozróżnienia w logach
    client_id = f"Client-{datetime.now().strftime('%H%M%S')}"
    
    # Adres serwera do połączenia
    server_address = "tcp://localhost:5555"
    
    # Liczba wiadomości do wysłania
    num_messages = 5
    
    # Opóźnienie między wiadomościami (sekundy)
    delay_between_messages = 1.0
    
    # 2. TWORZENIE KONTEKSTU ØMQ
    context = zmq.Context()
    logger.info("Utworzono kontekst ØMQ")
    
    # 3. TWORZENIE SOCKETA REQ (REQUEST)
    # REQ socket to część wzorca REQ-REP
    # Zawsze najpierw wysyła wiadomość, potem odbiera odpowiedź
    socket = context.socket(zmq.REQ)
    logger.info("Utworzono socket REQ")
    
    # 4. ŁĄCZENIE Z SERWEREM
    # connect() łączy socket z serwerem
    # Różnica między bind() a connect():
    # - bind() - socket nasłuchuje (serwer)
    # - connect() - socket inicjuje połączenie (klient)
    socket.connect(server_address)
    logger.info(f"Połączono z serwerem: {server_address}")
    
    try:
        # 5. GŁÓWNA PĘTLA KLIENTA
        logger.info(f"Wysyłanie {num_messages} wiadomości...")
        
        for i in range(1, num_messages + 1):
            # 6. PRZYGOTOWANIE WIADOMOŚCI
            message = f"Wiadomość numer {i} wysłana o {datetime.now().strftime('%H:%M:%S')}"
            
            # 7. WYSŁANIE WIADOMOŚCI I OTRZYMANIE ODPOWIEDZI
            response = send_message(socket, message, client_id)
            
            # 8. SPRAWDZENIE CZY OTRZYMANO ODPOWIEDŹ
            if response is None:
                logger.error(f"Nie udało się wysłać wiadomości {i}")
                break
            
            # 9. SEPARATOR DLA CZYTELNOŚCI
            logger.info("-" * 60)
            
            # 10. OPÓŹNIENIE MIĘDZY WIADOMOŚCIAMI
            # Ostatnia wiadomość nie potrzebuje opóźnienia
            if i < num_messages:
                logger.info(f"Oczekiwanie {delay_between_messages}s przed następną wiadomością...")
                time.sleep(delay_between_messages)
        
        # 11. PODSUMOWANIE
        logger.info(f"Wysłano wszystkie {num_messages} wiadomości")
    
    except KeyboardInterrupt:
        # 12. OBSŁUGA PRZERWANIA (Ctrl+C)
        logger.info("Otrzymano sygnał przerwania...")
    
    except Exception as e:
        # 13. OBSŁUGA BŁĘDÓW
        logger.error(f"Wystąpił błąd: {e}")
    
    finally:
        # 14. SPRZĄTANIE ZASOBÓW
        logger.info("Zamykanie klienta...")
        socket.close()
        context.term()
        logger.info("Klient zamknięty")

def interactive_mode():
    """
    Tryb interaktywny - pozwala użytkownikowi wprowadzać wiadomości
    """
    logger = logging.getLogger("InteractiveClient")
    logger.info("Uruchamianie trybu interaktywnego...")
    
    # Konfiguracja
    client_id = "Interactive"
    server_address = "tcp://localhost:5555"
    
    # Tworzenie połączenia
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect(server_address)
    logger.info(f"Połączono z serwerem: {server_address}")
    
    try:
        print("\n" + "="*50)
        print("TRYB INTERAKTYWNY")
        print("Wpisz wiadomość i naciśnij Enter")
        print("Wpisz 'quit' aby zakończyć")
        print("="*50)
        
        while True:
            # Pobierz wiadomość od użytkownika
            user_input = input("\nWiadomość: ").strip()
            
            # Sprawdź czy użytkownik chce zakończyć
            if user_input.lower() in ['quit', 'exit', 'q']:
                print("Zamykanie trybu interaktywnego...")
                break
            
            # Wyślij wiadomość jeśli nie jest pusta
            if user_input:
                response = send_message(socket, user_input, client_id)
                if response is None:
                    print("Błąd wysyłania wiadomości!")
                    break
            else:
                print("Pusta wiadomość - spróbuj ponownie")
    
    except KeyboardInterrupt:
        print("\nOtrzymano Ctrl+C - zamykanie...")
    
    finally:
        socket.close()
        context.term()
        logger.info("Tryb interaktywny zamknięty")

if __name__ == "__main__":
    # Sprawdź argumenty linii poleceń
    if len(sys.argv) > 1 and sys.argv[1] == "-i":
        interactive_mode()
    else:
        main()