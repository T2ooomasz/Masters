"""
Aplikacja testowa producent-konsument wykorzystujaca BoundedBuffer
z rozproszonym monitorem i pula monitorow (MonitorPool) dla watkow.
"""
import time
import random
from threading import Thread, current_thread
import sys
import os
from collections import deque

from monitor_client import MonitorError, DistributedMonitor
from bounded_buffer import BoundedBuffer

# Konfiguracja
SERVER_ADDRESS = "tcp://localhost:5555"
MONITOR_NAME_BB = "test_monitor_pooled" # Uzywamy innej nazwy monitora, aby uniknac kolizji
BUFFER_CAPACITY = 5
NUM_PRODUCERS = 2
NUM_CONSUMERS = 2
ITEMS_PER_PRODUCER = 10

def producer_thread(producer_id: int,
                    server_address: str,
                    monitor_name: str,
                    buffer_cap: int,
                    shared_buffer: deque,
                    num_items: int,
                    produced_items_list: list):
    """
    Watek producenta.
    """
    # Kazdy watek tworzy wlasna instancje klienta monitora
    monitor_client = DistributedMonitor(monitor_name, server_address)
    bb_instance = BoundedBuffer(buffer_cap, monitor_client, shared_buffer)
    try:
        # print(f"Producent {producer_id} (Watek: {current_thread().name}, Monitor Client PID: {bb_instance.monitor.process_id}) startuje...")
        for i in range(num_items):
            item = f"Item-{producer_id}-{i}"
            time.sleep(random.uniform(0.1, 0.5)) # Symulacja produkcji
            # print(f"Producent {producer_id} chce dodac: {item}")
            bb_instance.put(item)
            # print(f"Producent {producer_id} dodal: {item}")
            produced_items_list.append(item)
        # print(f"Producent {producer_id} zakonczyl.")
    finally:
        monitor_client.close() # Kazdy watek sam zamyka swoje polaczenie

def consumer_thread(consumer_id: int,
                    server_address: str,
                    monitor_name: str,
                    buffer_cap: int,
                    shared_buffer: deque,
                    num_items_to_consume: int,
                    consumed_items_list: list):
    """
    Watek konsumenta.
    """
    # Kazdy watek tworzy wlasna instancje klienta monitora
    monitor_client = DistributedMonitor(monitor_name, server_address)
    bb_instance = BoundedBuffer(buffer_cap, monitor_client, shared_buffer)
    try:
        # print(f"Konsument {consumer_id} (Watek: {current_thread().name}, Monitor Client PID: {bb_instance.monitor.process_id}) startuje...")
        for _ in range(num_items_to_consume):
            time.sleep(random.uniform(0.2, 0.8)) # Symulacja konsumpcji
            # print(f"Konsument {consumer_id} chce pobrac element...")
            item = bb_instance.get()
            # print(f"Konsument {consumer_id} pobral: {item}")
            consumed_items_list.append(item)
        # print(f"Konsument {consumer_id} zakonczyl.")
    finally:
        monitor_client.close() # Kazdy watek sam zamyka swoje polaczenie

if __name__ == "__main__":
    # Watki wspoldzielaja pamiec, wiec mozemy uzyc zwyklych list i deque
    produced_items = []
    consumed_items = []
    shared_buffer = deque()

    threads = []
    total_items_to_produce = NUM_PRODUCERS * ITEMS_PER_PRODUCER

    # Tworzenie i uruchamianie producentow
    for i in range(NUM_PRODUCERS):
        t = Thread(target=producer_thread, args=(
            i,
            SERVER_ADDRESS,
            MONITOR_NAME_BB,
            BUFFER_CAPACITY,
            shared_buffer,
            ITEMS_PER_PRODUCER,
            produced_items
        ))
        threads.append(t)
        t.start()

    # Tworzenie i uruchamianie konsumentow
    for i in range(NUM_CONSUMERS):
        items_for_this_consumer = total_items_to_produce // NUM_CONSUMERS
        if i < total_items_to_produce % NUM_CONSUMERS:
            items_for_this_consumer += 1

        t = Thread(target=consumer_thread, args=(
            i,
            SERVER_ADDRESS,
            MONITOR_NAME_BB,
            BUFFER_CAPACITY,
            shared_buffer,
            items_for_this_consumer,
            consumed_items
        ))
        threads.append(t)
        t.start()

    # Oczekiwanie na zakonczenie wszystkich watkow
    for t in threads:
        t.join()


    # Verification
    if sorted(produced_items) == sorted(consumed_items) and len(produced_items) == total_items_to_produce:
        print("SUCCESS")
    else:
        print("FAILURE")