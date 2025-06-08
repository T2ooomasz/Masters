#!/usr/bin/env python3
"""
Test Monitor - Etap 3
Testy distributed monitora z condition variables

Scenariusze testowe:
1. Podstawowe enter/exit
2. Wait/Signal między procesami
3. Producer-Consumer z condition variables
4. Concurrent access test
"""

import time
import threading
import multiprocessing
from monitor_client_step3 import MonitorClient
import logging

# Setup logowania
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)

def test_basic_enter_exit():
    """Test 1: Podstawowe wejście i wyjście z monitora."""
    print("\n=== Test 1: Basic Enter/Exit ===")
    
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        print("Wchodzę do monitora...")
        success = monitor.enter(timeout=5.0)
        if success:
            print("✓ Mutex uzyskany")
            time.sleep(1)  # Symulacja pracy
            monitor.exit()
            print("✓ Mutex zwolniony")
        else:
            print("✗ Nie udało się uzyskać mutex")
    finally:
        monitor.close()

# Globalny bufor symulowany przez pliki (bo procesy nie dzielą pamięci)
import os
import pickle

BUFFER_FILE = "tmp/monitor_buffer.pkl"
BUFFER_LOCK = "tmp/monitor_buffer.lock"

def save_buffer(items):
    """Zapisz bufor do pliku."""
    with open(BUFFER_FILE, 'wb') as f:
        pickle.dump(items, f)

def load_buffer():
    """Wczytaj bufor z pliku."""
    if os.path.exists(BUFFER_FILE):
        try:
            with open(BUFFER_FILE, 'rb') as f:
                return pickle.load(f)
        except:
            return []
    return []

def init_buffer():
    """Zainicjuj pusty bufor."""
    save_buffer([])

def producer_process(item_count: int, delay: float = 0.5):
    """
    Proces producenta - dodaje elementy do bufora.
    Używa condition variable do sygnalizacji.
    """
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        for i in range(item_count):
            monitor.enter()
            try:
                # Wczytaj aktualny bufor
                buffer = load_buffer()
                
                # Dodaj nowy element
                item = f"item_{i}"
                buffer.append(item)
                save_buffer(buffer)
                
                print(f"Producer: Dodałem {item}, bufor ma {len(buffer)} elementów")
                time.sleep(delay)  # Symulacja produkcji
                
                # Sygnalizuj że dodano element
                monitor.signal("item_available")
                
            finally:
                monitor.exit()
            
            time.sleep(0.1)  # Krótka przerwa między elementami
    
    except Exception as e:
        print(f"Producer error: {e}")
    finally:
        monitor.close()

def consumer_process(item_count: int):
    """
    Proces konsumenta - pobiera elementy z bufora.
    Używa wait() żeby czekać na dostępne elementy.
    """
    monitor = MonitorClient("tcp://localhost:5555")
    consumed = 0
    
    try:
        while consumed < item_count:
            monitor.enter()
            try:
                # Sprawdź czy są elementy w buforze
                buffer = load_buffer()
                
                if len(buffer) > 0:
                    # Pobierz element
                    item = buffer.pop(0)
                    save_buffer(buffer)
                    consumed += 1
                    
                    print(f"Consumer: Pobrałem {item}, zostało {len(buffer)} elementów")
                    time.sleep(0.2)  # Symulacja konsumpcji
                else:
                    # Bufor pusty - czekaj na nowe elementy
                    print(f"Consumer: Bufor pusty, czekam na item_available...")
                    
                    monitor.exit()  # Zwolnij mutex przed wait
                    
                    if monitor.wait("item_available", timeout=15.0):
                        print("Consumer: Obudzony, sprawdzam bufor ponownie...")
                        continue  # Sprawdź bufor ponownie
                    else:
                        print("Consumer: Timeout podczas wait")
                        break
                    
            finally:
                if monitor.has_mutex:  # Zwolnij tylko jeśli nadal mamy mutex
                    monitor.exit()
            
            time.sleep(0.1)
    
    except Exception as e:
        print(f"Consumer error: {e}")
    finally:
        monitor.close()
        print(f"Consumer skończył, skonsumował {consumed} elementów")

def waiter_process(condition_name: str, wait_time: float = 10.0):
    """Proces który czeka na warunek."""
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        monitor.enter()
        try:
            print(f"Waiter: Czekam na '{condition_name}'...")
            start_time = time.time()
            
            if monitor.wait(condition_name, timeout=wait_time):
                elapsed = time.time() - start_time
                print(f"Waiter: Obudzony po {elapsed:.2f}s!")
            else:
                print(f"Waiter: Timeout po {wait_time}s")
                
        finally:
            monitor.exit()
    
    except Exception as e:
        print(f"Waiter error: {e}")
    finally:
        monitor.close()

def signaler_process(condition_name: str, delay: float = 2.0):
    """Proces który sygnalizuje warunek po opóźnieniu."""
    time.sleep(delay)  # Poczekaj żeby waiter zdążył się ustawić
    
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        monitor.enter()
        try:
            print(f"Signaler: Sygnalizuję '{condition_name}'")
            monitor.signal(condition_name)
        finally:
            monitor.exit()
    
    except Exception as e:
        print(f"Signaler error: {e}")
    finally:
        monitor.close()

def test_wait_signal():
    """Test 2: Wait/Signal między procesami."""
    print("\n=== Test 2: Wait/Signal ===")
    
    # Proces 1: czeka na warunek
    waiter = multiprocessing.Process(
        target=waiter_process, 
        args=("test_condition", 5.0)
    )
    
    # Proces 2: sygnalizuje warunek po 2 sekundach
    signaler = multiprocessing.Process(
        target=signaler_process,
        args=("test_condition", 2.0)
    )
    
    waiter.start()
    signaler.start()
    
    waiter.join()
    signaler.join()
    
    print("✓ Test wait/signal zakończony")

def test_producer_consumer():
    """Test 3: Producer-Consumer z condition variables."""
    print("\n=== Test 3: Producer-Consumer ===")
    
    ITEM_COUNT = 3
    
    # Zainicjuj pusty bufor
    print("Inicjalizuję pusty bufor...")
    init_buffer()
    
    # Uruchom producenta i konsumenta
    producer = multiprocessing.Process(
        target=producer_process,
        args=(ITEM_COUNT, 0.5)
    )
    
    consumer = multiprocessing.Process(
        target=consumer_process,
        args=(ITEM_COUNT,)
    )
    
    print(f"Uruchamiam producenta ({ITEM_COUNT} elementów) i konsumenta...")
    
    producer.start()
    consumer.start()
    
    producer.join()
    consumer.join()
    
    # Sprawdź końcowy stan bufora
    final_buffer = load_buffer()
    print(f"Końcowy stan bufora: {final_buffer}")
    
    # Wyczyść bufor
    if os.path.exists(BUFFER_FILE):
        os.remove(BUFFER_FILE)
    
    print("✓ Test producer-consumer zakończony")

def concurrent_worker(worker_id: int, iterations: int):
    """Worker dla testu concurrent access."""
    monitor = MonitorClient("tcp://localhost:5555")
    
    try:
        for i in range(iterations):
            monitor.enter()
            try:
                print(f"Worker-{worker_id}: Iteracja {i+1}/{iterations}")
                time.sleep(0.1)  # Symulacja pracy w sekcji krytycznej
            finally:
                monitor.exit()
            
            time.sleep(0.05)  # Krótka przerwa
    
    except Exception as e:
        print(f"Worker-{worker_id} error: {e}")
    finally:
        monitor.close()

def test_concurrent_access():
    """Test 4: Konkurencyjny dostęp wielu procesów."""
    print("\n=== Test 4: Concurrent Access ===")
    
    WORKER_COUNT = 3
    ITERATIONS = 3
    
    workers = []
    
    # Uruchom workerów
    for i in range(WORKER_COUNT):
        worker = multiprocessing.Process(
            target=concurrent_worker,
            args=(i, ITERATIONS)
        )
        workers.append(worker)
        worker.start()
    
    # Poczekaj na wszystkich
    for worker in workers:
        worker.join()
    
    print("✓ Test concurrent access zakończony")

def test_broadcast():
    """Test 5: Broadcast - obudź wszystkie procesy."""
    print("\n=== Test 5: Broadcast ===")
    
    WAITER_COUNT = 3
    
    # Uruchom kilku waiterów
    waiters = []
    for i in range(WAITER_COUNT):
        waiter = multiprocessing.Process(
            target=waiter_process,
            args=(f"broadcast_test", 8.0)
        )
        waiters.append(waiter)
        waiter.start()
    
    # Poczekaj żeby się ustawili
    time.sleep(1.0)
    
    # Broadcaster - obudzi wszystkich
    def broadcaster():
        monitor = MonitorClient("tcp://localhost:5555")
        try:
            monitor.enter()
            try:
                print("Broadcaster: Obudzę wszystkich (broadcast)")
                monitor.broadcast("broadcast_test")
            finally:
                monitor.exit()
        finally:
            monitor.close()
    
    broadcast_proc = multiprocessing.Process(target=broadcaster)
    broadcast_proc.start()
    
    # Poczekaj na wszystkich
    for waiter in waiters:
        waiter.join()
    broadcast_proc.join()
    
    print("✓ Test broadcast zakończony")

def show_server_status():
    """Pokaż status serwera."""
    print("\n=== Status Serwera ===")
    
    monitor = MonitorClient("tcp://localhost:5555")
    try:
        status = monitor.get_server_status()
        print(f"Mutex owner: {status.get('mutex_owner', 'None')}")
        print(f"Mutex queue: {status.get('mutex_queue', [])}")
        print(f"Condition queues: {status.get('condition_queues', {})}")
        print(f"Waiting processes: {status.get('waiting_processes', [])}")
    except Exception as e:
        print(f"Nie udało się pobrać statusu: {e}")
    finally:
        monitor.close()

if __name__ == "__main__":
    print("DISTRIBUTED MONITOR - TESTY ETAP 3")
    print("=" * 50)
    print("UWAGA: Przed uruchomieniem testów, uruchom serwer:")
    print("python monitor_server_step3.py")
    print("=" * 50)
    
    try:
        # Sprawdź czy serwer działa
        monitor = MonitorClient("tcp://localhost:5555")
        monitor.enter(timeout=2.0)
        monitor.exit()
        monitor.close()
        print("✓ Serwer jest dostępny\n")
    except:
        print("✗ Serwer niedostępny - uruchom monitor_server_step3.py")
        exit(1)
    
    # Uruchom testy
    test_basic_enter_exit()
    show_server_status()
    
    test_wait_signal()
    show_server_status()
    
    test_producer_consumer()
    show_server_status()
    
    test_concurrent_access()
    show_server_status()
    
    test_broadcast()
    show_server_status()
    
    print("\n" + "=" * 50)
    print("WSZYSTKIE TESTY ZAKOŃCZONE")
    print("Jeśli nie było błędów - monitor działa poprawnie!")
    print("=" * 50)