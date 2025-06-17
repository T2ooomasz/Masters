"""
Porównanie wydajności BoundedBuffer z rozproszonym monitorem
i lokalnym monitorem (threading.Lock/Condition).
"""

import time
import random
import threading
import multiprocessing as mp
import sys
import os

# Dodaj ścieżkę do katalogu nadrzędnego
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    from monitor_client import DistributedMonitor
    from bounded_buffer import BoundedBuffer # Nasz BoundedBuffer z rozproszonym monitorem
except ImportError:
    print("Nie można zaimportować modułów. Upewnij się, że monitor_client.py i bounded_buffer.py są w PYTHONPATH.")
    sys.exit(1)


class LocalBoundedBuffer:
    """
    Implementacja BoundedBuffer z lokalnym monitorem (threading.Lock/Condition).
    """
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.buffer = []
        self.lock = threading.Lock()
        self.not_full = threading.Condition(self.lock)
        self.not_empty = threading.Condition(self.lock)

    def put(self, item):
        with self.lock:
            while len(self.buffer) >= self.capacity:
                self.not_full.wait()
            self.buffer.append(item)
            self.not_empty.notify()

    def get(self):
        with self.lock:
            while len(self.buffer) == 0:
                self.not_empty.wait()
            item = self.buffer.pop(0)
            self.not_full.notify()
            return item

def producer_consumer_test(buffer_class, num_producers, num_consumers, items_per_producer, shared_data):
    """
    Funkcja testująca producentów i konsumentów dla danej klasy bufora.
    """
    if buffer_class == BoundedBuffer: # Dla rozproszonego musimy przekazać monitor i współdzielony bufor
        monitor = DistributedMonitor("performance_test_monitor", "tcp://localhost:5555")
        buffer = buffer_class(shared_data["capacity"], monitor, shared_data["buffer"])
    else: # Dla lokalnego tylko pojemność
        buffer = buffer_class(shared_data["capacity"])
    
    processes = []
    total_items = num_producers * items_per_producer

    # Producenci
    for i in range(num_producers):
        p = mp.Process(target=producer, args=(buffer, items_per_producer, i))
        processes.append(p)
        p.start()

    # Konsumenci
    for i in range(num_consumers):
        p = mp.Process(target=consumer, args=(buffer, total_items // num_consumers, i))
        processes.append(p)
        p.start()

    for p in processes:
        p.join()
    
    if buffer_class == BoundedBuffer:
        monitor.close()

def producer(buffer, num_items, producer_id):
    for i in range(num_items):
        item = (producer_id, i)
        buffer.put(item)

def consumer(buffer, num_items, consumer_id):
    for _ in range(num_items):
        buffer.get()

if __name__ == "__main__":
    num_runs = 3
    capacity = 10
    num_producers = 2
    num_consumers = 2
    items_per_producer = 1000

    # Przygotowanie danych współdzielonych (zarządzanych przez multiprocessing.Manager)
    manager = mp.Manager()
    shared_data = {
        "capacity": capacity,
        "buffer": manager.list() # Używamy listy jako proxy dla współdzielonego bufora
    }
    
    print("Test wydajności: Porównanie BoundedBuffer (rozproszony vs lokalny)")
    print(f"Pojemność bufora: {capacity}, Producenci: {num_producers}, Konsumenci: {num_consumers}, Elementy/Producent: {items_per_producer}")
    
    distributed_times = []
    for i in range(num_runs):
        start_time = time.time()
        producer_consumer_test(BoundedBuffer, num_producers, num_consumers, items_per_producer, shared_data)
        end_time = time.time()
        distributed_times.append(end_time - start_time)
        print(f"Czas wykonania (rozproszony, run {i+1}): {end_time - start_time:.4f} s")

    local_times = []
    for i in range(num_runs):
        start_time = time.time()
        producer_consumer_test(LocalBoundedBuffer, num_producers, num_consumers, items_per_producer, shared_data)
        end_time = time.time()
        local_times.append(end_time - start_time)
        print(f"Czas wykonania (lokalny, run {i+1}): {end_time - start_time:.4f} s")
