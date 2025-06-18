#!/usr/bin/env python3
"""
Hello Server - Podstawowy serwer ØMQ
Demonstracja wzorca REQ-REP (Request-Reply)

Ten serwer:
1. Nasłuchuje na porcie 5555
2. Czeka na wiadomości od klientów
3. Odpowiada na każdą wiadomość
4. Loguje wszystkie operacje
"""

import zmq
import time
import logging
from datetime import datetime

# Konfiguracja logowania dla lepszej widoczności działania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(levelname)s - %(message)s',
    datefmt='%H:%M:%S'
)

def main():
    """
    Główna funkcja serwera
    """
    logger = logging.getLogger("HelloServer")
    logger.info("Uruchamianie Hello Server...")
    
    # 1. TWORZENIE KONTEKSTU ØMQ
    # Context to główny obiekt ØMQ zarządzający wszystkimi socketami
    # Jeden context może obsługiwać wiele socketów
    context = zmq.Context()
    logger.info("Utworzono kontekst ØMQ")
    
    # 2. TWORZENIE SOCKETA REP (REPLY)
    # REP socket to część wzorca REQ-REP
    # Zawsze najpierw odbiera wiadomość, potem wysyła odpowiedź
    socket = context.socket(zmq.REP)
    logger.info("Utworzono socket REP")
    
    # 3. BINDOWANIE SOCKETA DO ADRESU
    # bind() sprawia, że socket nasłuchuje na danym adresie
    # "tcp://*:5555" oznacza:
    # - tcp:// - protokół TCP
    # - * - wszystkie interfejsy sieciowe (0.0.0.0)
    # - 5555 - numer portu
    socket.bind("tcp://*:5555")
    logger.info("Serwer nasłuchuje na tcp://*:5555")
    
    # 4. LICZNIK WIADOMOŚCI
    # Zmienna do śledzenia ilości obsłużonych wiadomości
    message_count = 0
    
    try:
        # 5. GŁÓWNA PĘTLA SERWERA
        # Pętla nieskończona obsługująca wiadomości
        logger.info("Serwer gotowy do obsługi klientów...")
        
        while True:
            # 6. ODBIERANIE WIADOMOŚCI
            # recv_string() blokuje wykonanie do momentu otrzymania wiadomości
            # Automatycznie dekoduje bytes na string (UTF-8)
            logger.info("Oczekiwanie na wiadomość...")
            message = socket.recv_string()
            
            # 7. ZWIĘKSZANIE LICZNIKA
            message_count += 1
            
            # 8. LOGOWANIE OTRZYMANEJ WIADOMOŚCI
            logger.info(f"Otrzymano wiadomość #{message_count}: '{message}'")
            
            # 9. SYMULACJA PRZETWARZANIA
            # Krótkie opóźnienie symulujące pracę serwera
            # W rzeczywistym monitorze tutaj byłaby logika zarządzania stanem
            processing_time = 0.1  # 100ms
            time.sleep(processing_time)
            
            # 10. PRZYGOTOWANIE ODPOWIEDZI
            # Tworzymy odpowiedź zawierającą:
            # - Potwierdzenie otrzymania
            # - Numer wiadomości
            # - Timestamp
            response = f"Hello! Otrzymałem wiadomość #{message_count}: '{message}' o {datetime.now().strftime('%H:%M:%S')}"
            
            # 11. WYSYŁANIE ODPOWIEDZI
            # send_string() wysyła odpowiedź do klienta
            # W wzorcu REP musimy wysłać dokładnie jedną odpowiedź na każde żądanie
            socket.send_string(response)
            logger.info(f"Wysłano odpowiedź #{message_count}")
            
            # 12. SEPARATOR DLA CZYTELNOŚCI LOGÓW
            logger.info("-" * 50)
    
    except KeyboardInterrupt:
        # 13. OBSŁUGA PRZERWANIA (Ctrl+C)
        logger.info("Otrzymano sygnał przerwania...")
    
    except Exception as e:
        # 14. OBSŁUGA BŁĘDÓW
        logger.error(f"Wystąpił błąd: {e}")
    
    finally:
        # 15. SPRZĄTANIE ZASOBÓW
        # Zawsze należy zamknąć socket i context
        logger.info("Zamykanie serwera...")
        socket.close()
        context.term()
        logger.info("Serwer zamknięty")

if __name__ == "__main__":
    main()