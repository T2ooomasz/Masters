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
    def __init__(self, capacity: int, shared_buffer_proxy=None): # Dodajemy shared_buffer_proxy
        self.capacity = capacity
        # Jeśli shared_buffer_proxy nie jest dostarczony (np. dla testów jednowątkowych/jednoprocesowych bez Managera),
        # tworzymy zwykłą listę. W kontekście tego testu zawsze będzie dostarczony.
        self.buffer = shared_buffer_proxy if shared_buffer_proxy is not None else []
        self.lock = mp.Lock() # Używamy multiprocessing.Lock
        self.not_full = mp.Condition(self.lock) # Używamy multiprocessing.Condition
        self.not_empty = mp.Condition(self.lock)

    def put(self, item):
        # print(f"PID {os.getpid()} Local PUT: Trying for item {item}. Buf len: {len(self.buffer)}")
        with self.lock:
            # print(f"PID {os.getpid()} Local PUT: Lock acquired for item {item}. Buf len: {len(self.buffer)}")
            while len(self.buffer) >= self.capacity:
                # print(f"PID {os.getpid()} Local PUT: Buffer full, waiting on not_full. Buf len: {len(self.buffer)}")
                self.not_full.wait()
                # print(f"PID {os.getpid()} Local PUT: Awakened from not_full. Buf len: {len(self.buffer)}")
            self.buffer.append(item)
            # print(f"PID {os.getpid()} Local PUT: Item {item} appended. Buf len: {len(self.buffer)}. Notifying not_empty.")
            self.not_empty.notify_all() # Zmiana na notify_all dla większej pewności
        # print(f"PID {os.getpid()} Local PUT: Lock released for item {item}")

    def get(self):
        # print(f"PID {os.getpid()} Local GET: Trying to get item. Buf len: {len(self.buffer)}")
        with self.lock:
            # print(f"PID {os.getpid()} Local GET: Lock acquired. Buf len: {len(self.buffer)}")
            while len(self.buffer) == 0:
                # print(f"PID {os.getpid()} Local GET: Buffer empty, waiting on not_empty. Buf len: {len(self.buffer)}")
                self.not_empty.wait()
                # print(f"PID {os.getpid()} Local GET: Awakened from not_empty. Buf len: {len(self.buffer)}")
            item = self.buffer.pop(0)
            # print(f"PID {os.getpid()} Local GET: Item {item} popped. Buf len: {len(self.buffer)}. Notifying not_full.")
            self.not_full.notify_all() # Zmiana na notify_all dla większej pewności
            return item
        # print(f"PID {os.getpid()} Local GET: Lock released after getting item")

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
    # Zarówno lokalny (wieloprocesowy) jak i rozproszony będą używać współdzielonej listy
    shared_actual_buffer = manager.list()

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
        buffer = LocalBoundedBuffer(capacity, shared_buffer_proxy) # Przekazujemy shared_buffer_proxy

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
        buffer = LocalBoundedBuffer(capacity, shared_buffer_proxy) # Przekazujemy shared_buffer_proxy

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