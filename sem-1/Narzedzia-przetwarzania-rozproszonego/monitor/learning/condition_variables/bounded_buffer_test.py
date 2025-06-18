#!/usr/bin/env python3
"""
Bounded Buffer - Test Producer-Consumer z Distributed Monitor
Klasyczny problem synchronizacji: producenci dodają elementy do bufora,
konsumenci je pobierają. Buffer ma ograniczoną pojemność.

Demonstruje użycie condition variables:
- "not_full" - producenci czekają gdy buffer pełny
- "not_empty" - konsumenci czekają gdy buffer pusty

Architektura:
[Producer 1] ←→ [Monitor Server] ←→ [Consumer 1]
[Producer 2] ←→  (Shared Buffer)  ←→ [Consumer 2]
[Producer N] ←→                   ←→ [Consumer M]
"""

import time
import threading
import random
import argparse
import sys
from typing import List, Any
from monitor_client_step3 import DistributedMonitor, DistributedMonitorError

class DistributedBoundedBuffer:
    """
    Distributed Bounded Buffer używający monitora rozproszonego.
    
    Buffer jest przechowywany lokalnie, ale dostęp do niego jest
    synchronizowany przez distributed monitor. Każdy proces/wątek
    ma własną kopię bufora, ale operacje są atomiczne dzięki monitorowi.
    
    W rzeczywistej implementacji buffer byłby przechowywany w bazie danych
    lub shared memory, ale dla celów demonstracyjnych używamy lokalnej kopii.
    """
    
    def __init__(self, capacity: int, server_address: str = "tcp://localhost:5555"):
        self.capacity = capacity
        self.buffer: List[Any] = []  # Lokalny buffer (w rzeczywistości byłby współdzielony)
        self.monitor = DistributedMonitor(server_address)
        
        # Nazwy warunków
        self.NOT_FULL = "not_full"    # Buffer nie jest pełny
        self.NOT_EMPTY = "not_empty"  # Buffer nie jest pusty
        
        print(f"BoundedBuffer utworzony: pojemność={capacity}")
    
    def put(self, item: Any, producer_id: str):
        """
        Dodanie elementu do bufora (Producer).
        
        Logika:
        1. Wejdź do monitora
        2. Czekaj dopóki buffer nie jest pełny
        3. Dodaj element
        4. Sygnalizuj konsumentom (not_empty)
        5. Wyjdź z monitora
        """
        try:
            self.monitor.enter()
            
            # Czekaj dopóki buffer nie jest pełny
            while len(self.buffer) >= self.capacity:
                print(f"[{producer_id}] Buffer pełny ({len(self.buffer)}/{self.capacity}), czekam...")
                self.monitor.wait(self.NOT_FULL)
            
            # Dodaj element do bufora
            self.buffer.append(item)
            print(f"[{producer_id}] Dodano '{item}', buffer: {len(self.buffer)}/{self.capacity}")
            
            # Sygnalizuj konsumentom że buffer nie jest pusty
            self.monitor.signal(self.NOT_EMPTY)
            
        finally:
            self.monitor.exit()
    
    def get(self, consumer_id: str) -> Any:
        """
        Pobranie elementu z bufora (Consumer).
        
        Logika:
        1. Wejdź do monitora
        2. Czekaj dopóki buffer nie jest pusty
        3. Pobierz element
        4. Sygnalizuj producentom (not_full)
        5. Wyjdź z monitora
        """
        try:
            self.monitor.enter()
            
            # Czekaj dopóki buffer nie jest pusty
            while len(self.buffer) == 0:
                print(f"[{consumer_id}] Buffer pusty, czekam...")
                self.monitor.wait(self.NOT_EMPTY)
            
            # Pobierz element z bufora
            item = self.buffer.pop(0)  # FIFO
            print(f"[{consumer_id}] Pobrałem '{item}', buffer: {len(self.buffer)}/{self.capacity}")
            
            # Sygnalizuj producentom że buffer nie jest pełny
            self.monitor.signal(self.NOT_FULL)
            
            return item
            
        finally:
            self.monitor.exit()
    
    def size(self) -> int:
        """Zwraca aktualny rozmiar bufora (bez synchronizacji - tylko do debugowania)"""
        return len(self.buffer)
    
    def disconnect(self):
        """Rozłączenie z monitorem"""
        self.monitor.disconnect()

def producer_worker(buffer: DistributedBoundedBuffer, producer_id: str, 
                   num_items: int, delay_range: tuple):
    """
    Funkcja producenta - dodaje elementy do bufora.
    
    Args:
        buffer: Współdzielony buffer
        producer_id: Identyfikator producenta
        num_items: Liczba elementów do wyprodukowania
        delay_range: Zakres opóźnień między produkcją (min, max) w sekundach
    """
    print(f"[{producer_id}] Rozpoczynam produkcję {num_items} elementów")
    
    try:
        for i in range(num_items):
            # Wyprodukuj element
            item = f"{producer_id}_item_{i+1}"
            
            # Dodaj do bufora
            buffer.put(item, producer_id)
            
            # Symulacja czasu produkcji
            delay = random.uniform(*delay_range)
            time.sleep(delay)
            
    except DistributedMonitorError as e:
        print(f"[{producer_id}] Błąd: {e}")
    except Exception as e:
        print(f"[{producer_id}] Nieoczekiwany błąd: {e}")
    
    print(f"[{producer_id}] Zakończono produkcję")

def consumer_worker(buffer: DistributedBoundedBuffer, consumer_id: str, 
                   num_items: int, delay_range: tuple):
    """
    Funkcja konsumenta - pobiera elementy z bufora.
    
    Args:
        buffer: Współdzielony buffer
        consumer_id: Identyfikator konsumenta
        num_items: Liczba elementów do skonsumowania
        delay_range: Zakres opóźnień między konsumpcją (min, max) w sekundach
    """
    print(f"[{consumer_id}] Rozpoczynam konsumpcję {num_items} elementów")
    
    try:
        for i in range(num_items):
            # Pobierz z bufora
            item = buffer.get(consumer_id)
            
            # Symulacja przetwarzania
            delay = random.uniform(*delay_range)
            print(f"[{consumer_id}] Przetwarzam '{item}' przez {delay:.2f}s")
            time.sleep(delay)
            
    except DistributedMonitorError as e:
        print(f"[{consumer_id}] Błąd: {e}")
    except Exception as e:
        print(f"[{consumer_id}] Nieoczekiwany błąd: {e}")
    
    print(f"[{consumer_id}] Zakończono konsumpcję")

def run_test(buffer_size: int, num_producers: int, num_consumers: int,
             items_per_producer: int, items_per_consumer: int,
             server_address: str = "tcp://localhost:5555"):
    """
    Uruchomienie testu Producer-Consumer.
    
    Args:
        buffer_size: Pojemność bufora
        num_producers: Liczba producentów
        num_consumers: Liczba konsumentów
        items_per_producer: Elementy na producenta
        items_per_consumer: Elementy na konsumenta
        server_address: Adres serwera monitora
    """
    print(f"""
=== BOUNDED BUFFER TEST ===
Buffer size: {buffer_size}
Producers: {num_producers} (każdy produkuje {items_per_producer} elementów)
Consumers: {num_consumers} (każdy konsumuje {items_per_consumer} elementów)
Total items: {num_producers * items_per_producer} produced, {num_consumers * items_per_consumer} consumed
Server: {server_address}
    """)
    
    # Sprawdzenie balansu
    total_produced = num_producers * items_per_producer
    total_consumed = num_consumers * items_per_consumer
    
    if total_produced != total_consumed:
        print(f"UWAGA: Niezbalansowana produkcja/konsumpcja ({total_produced} vs {total_consumed})")
    
    # Utworzenie współdzielonego bufora
    buffer = DistributedBoundedBuffer(buffer_size, server_address)
    
    # Lista wątków
    threads = []
    
    try:
        # Uruchomienie producentów
        for i in range(num_producers):
            producer_id = f"Producer-{i+1}"
            thread = threading.Thread(
                target=producer_worker,
                args=(buffer, producer_id, items_per_producer, (0.1, 0.5))
            )
            thread.start()
            threads.append(thread)
        
        # Krótkie opóźnienie przed uruchomieniem konsumentów
        time.sleep(0.2)
        
        # Uruchomienie konsumentów
        for i in range(num_consumers):
            consumer_id = f"Consumer-{i+1}"
            thread = threading.Thread(
                target=consumer_worker,
                args=(buffer, consumer_id, items_per_consumer, (0.2, 0.8))
            )
            thread.start()
            threads.append(thread)
        
        # Oczekiwanie na zakończenie wszystkich wątków
        print("\nOczekiwanie na zakończenie wszystkich workers...")
        for thread in threads:
            thread.join()
        
        print(f"\n=== TEST ZAKOŃCZONY ===")
        print(f"Końcowy rozmiar bufora: {buffer.size()}")
        
        # Pobranie stanu serwera
        try:
            status = buffer.monitor.get_server_status()
            print(f"Stan serwera: {status}")
        except Exception as e:
            print(f"Nie można pobrać stanu serwera: {e}")
    
    finally:
        # Cleanup
        buffer.disconnect()

def main():
    """Główna funkcja z obsługą argumentów"""
    parser = argparse.ArgumentParser(description='Bounded Buffer Test - Distributed Monitor')
    
    # Parametry testu
    parser.add_argument('--buffer-size', type=int, default=3, 
                       help='Pojemność bufora (domyślnie: 3)')
    parser.add_argument('--producers', type=int, default=2, 
                       help='Liczba producentów (domyślnie: 2)')
    parser.add_argument('--consumers', type=int, default=2, 
                       help='Liczba konsumentów (domyślnie: 2)')
    parser.add_argument('--items-per-producer', type=int, default=5, 
                       help='Elementy na producenta (domyślnie: 5)')
    parser.add_argument('--items-per-consumer', type=int, default=5, 
                       help='Elementy na konsumenta (domyślnie: 5)')
    parser.add_argument('--server', type=str, default='tcp://localhost:5555',
                       help='Adres serwera monitora (domyślnie: tcp://localhost:5555)')
    
    args = parser.parse_args()
    
    # Walidacja parametrów
    if args.buffer_size <= 0:
        print("Błąd: Rozmiar bufora musi być > 0")
        sys.exit(1)
    
    if args.producers <= 0 or args.consumers <= 0:
        print("Błąd: Liczba producers/consumers musi być > 0")
        sys.exit(1)
    
    if args.items_per_producer <= 0 or args.items_per_consumer <= 0:
        print("Błąd: Liczba elementów musi być > 0")
        sys.exit(1)
    
    try:
        run_test(
            buffer_size=args.buffer_size,
            num_producers=args.producers,
            num_consumers=args.consumers,
            items_per_producer=args.items_per_producer,
            items_per_consumer=args.items_per_consumer,
            server_address=args.server
        )
    except KeyboardInterrupt:
        print("\nTest przerwany przez użytkownika")
    except Exception as e:
        print(f"Błąd testu: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()