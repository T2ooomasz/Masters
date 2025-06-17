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

def producer_consumer_test(buffer_type_str, num_producers, num_consumers, items_per_producer, capacity, server_address, monitor_name_prefix):
    """
    Funkcja testująca producentów i konsumentów dla danej klasy bufora.

    Args:
        buffer_type_str: "distributed" lub "local"
        num_producers: liczba producentów
        num_consumers: liczba konsumentów
        items_per_producer: liczba elementów produkowanych przez każdego producenta
        capacity: pojemność bufora
        server_address: adres serwera monitora (dla "distributed")
        monitor_name_prefix: prefix nazwy monitora (dla "distributed")
    """
    processes = []
    total_items = num_producers * items_per_producer

    # Dla rozproszonego bufora, potrzebujemy współdzielonej listy dla samego bufora
    manager = mp.Manager()
    shared_actual_buffer = manager.list() if buffer_type_str == "distributed" else None

    # Producenci
    for i in range(num_producers):
        p = mp.Process(target=producer_worker, args=(
            buffer_type_str, capacity, items_per_producer, i, 
            server_address, f"{monitor_name_prefix}_prodcons", shared_actual_buffer
        ))
        processes.append(p)
        p.start()

    # Konsumenci
    items_per_consumer = total_items // num_consumers
    extra_items = total_items % num_consumers

    for i in range(num_consumers):
        items_to_consume = items_per_consumer + (1 if i < extra_items else 0)
        p = mp.Process(target=consumer_worker, args=(
            buffer_type_str, capacity, items_to_consume, i,
            server_address, f"{monitor_name_prefix}_prodcons", shared_actual_buffer
        ))
        processes.append(p)
        p.start()

    for p in processes:
        p.join()

def producer_worker(buffer_type_str, capacity, num_items, producer_id, server_address, monitor_name, shared_buffer_proxy):
    if buffer_type_str == "distributed":
        monitor = DistributedMonitor(monitor_name, server_address)
        buffer = BoundedBuffer(capacity, monitor, shared_buffer_proxy)
    else: # local
        buffer = LocalBoundedBuffer(capacity)

    for i in range(num_items):
        item = (producer_id, i)
        buffer.put(item)
    
    if buffer_type_str == "distributed":
        monitor.close()

def consumer_worker(buffer_type_str, capacity, num_items, consumer_id, server_address, monitor_name, shared_buffer_proxy):
    if buffer_type_str == "distributed":
        monitor = DistributedMonitor(monitor_name, server_address)
        buffer = BoundedBuffer(capacity, monitor, shared_buffer_proxy)
    else: # local
        buffer = LocalBoundedBuffer(capacity)

    for _ in range(num_items):
        buffer.get()

    if buffer_type_str == "distributed":
        monitor.close()

if __name__ == "__main__":
    num_runs = 3
    capacity = 10
    num_producers = 2
    num_consumers = 2
    items_per_producer = 1000
    server_address = "tcp://localhost:5555"
    monitor_name_base = "perf_test_monitor"

    print("Test wydajności: Porównanie BoundedBuffer (rozproszony vs lokalny)")
    print(f"Pojemność bufora: {capacity}, Producenci: {num_producers}, Konsumenci: {num_consumers}, Elementy/Producent: {items_per_producer}")
    
    distributed_times = []
    for i in range(num_runs):
        start_time = time.time()
        # Dla każdego uruchomienia testu rozproszonego, używamy unikalnej nazwy monitora,
        # aby uniknąć konfliktów stanu na serwerze, jeśli serwer nie jest resetowany.
        # Lub upewniamy się, że shared_actual_buffer jest czyszczony.
        # W tym przypadku, ponieważ shared_actual_buffer jest tworzony wewnątrz producer_consumer_test,
        # jest on świeży dla każdego wywołania tej funkcji.
        producer_consumer_test("distributed", num_producers, num_consumers, items_per_producer, capacity, server_address, f"{monitor_name_base}_dist_run{i}")
        end_time = time.time()
        distributed_times.append(end_time - start_time)
        print(f"Czas wykonania (rozproszony, run {i+1}): {end_time - start_time:.4f} s")

    local_times = []
    for i in range(num_runs):
        start_time = time.time()
        # Dla testu lokalnego, server_address i monitor_name nie są używane, ale przekazujemy je dla spójności API
        # shared_actual_buffer również nie jest używany przez LocalBoundedBuffer
        producer_consumer_test("local", num_producers, num_consumers, items_per_producer, capacity, server_address, f"{monitor_name_base}_local_run{i}")
        end_time = time.time()
        local_times.append(end_time - start_time)
        print(f"Czas wykonania (lokalny, run {i+1}): {end_time - start_time:.4f} s")