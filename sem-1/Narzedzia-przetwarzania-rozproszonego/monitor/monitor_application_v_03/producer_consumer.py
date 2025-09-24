"""
Aplikacja testowa producent-konsument wykorzystujaca BoundedBuffer
z rozproszonym monitorem.
"""
import time
import random
from multiprocessing import Process, current_process, Manager
import sys
import os
from monitor_client import DistributedMonitor, MonitorError, MonitorPool
from bounded_buffer import BoundedBuffer
# Dodaj sciezke do katalogu nadrzednego
# sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))


# Konfiguracja
SERVER_ADDRESS = "tcp://localhost:5555" # Upewnij sie, ze serwer monitora dziala pod tym adresem
MONITOR_NAME_BB = "test_monitor"
BUFFER_CAPACITY = 5
NUM_PRODUCERS = 2
NUM_CONSUMERS = 2
ITEMS_PER_PRODUCER = 10

def producer_process(producer_id: int, 
                     monitor_name: str, 
                     server_addr: str, 
                     buffer_cap: int, 
                     shared_buffer_proxy, # Dodajemy wspoldzielony bufor jako argument
                     num_items: int, 
                     produced_items_list):
    """
    Proces producenta.
    """
    # Kazdy proces tworzy wlasnego klienta monitora i BoundedBuffer
    # ale BoundedBuffer uzywa teraz wspoldzielonego obiektu kolejki
    monitor_client = DistributedMonitor(monitor_name, server_addr)
    bb_instance = BoundedBuffer(buffer_cap, monitor_client, shared_buffer_proxy)
    print(f"Producent {producer_id} (PID: {current_process().pid}, Monitor Client PID: {bb_instance.monitor.process_id}) startuje...")
    for i in range(num_items):
        item = f"Item-{producer_id}-{i}"
        time.sleep(random.uniform(0.1, 0.5)) # Symulacja produkcji
        print(f"Producent {producer_id} chce dodac: {item}")
        bb_instance.put(item)
        print(f"Producent {producer_id} dodal: {item}")
        produced_items_list.append(item)
    print(f"Producent {producer_id} zakonczyl.")
    monitor_client.close() # Jawne zamkniecie monitora w procesie potomnym

def consumer_process(consumer_id: int, 
                     monitor_name: str, 
                     server_addr: str, 
                     buffer_cap: int, 
                     shared_buffer_proxy, # Dodajemy wspoldzielony bufor jako argument
                     num_items_to_consume: int, 
                     consumed_items_list):
    """
    Proces konsumenta.
    """
    # Kazdy proces tworzy wlasnego klienta monitora i BoundedBuffer
    monitor_client = DistributedMonitor(monitor_name, server_addr)
    bb_instance = BoundedBuffer(buffer_cap, monitor_client, shared_buffer_proxy)
    print(f"Konsument {consumer_id} (PID: {current_process().pid}, Monitor Client PID: {bb_instance.monitor.process_id}) startuje...")
    for _ in range(num_items_to_consume):
        time.sleep(random.uniform(0.2, 0.8)) # Symulacja konsumpcji
        print(f"Konsument {consumer_id} chce pobrac element...")
        item = bb_instance.get()
        print(f"Konsument {consumer_id} pobral: {item}")
        consumed_items_list.append(item)
    print(f"Konsument {consumer_id} zakonczyl.")
    monitor_client.close() # Jawne zamkniecie monitora w procesie potomnym

if __name__ == "__main__":
    print("Uruchamianie aplikacji Producent-Konsument z rozproszonym monitorem...")
    print(f"Adres serwera monitora: {SERVER_ADDRESS}")
    print(f"Nazwa monitora dla BoundedBuffer: {MONITOR_NAME_BB}")
    print(f"Pojemnosc bufora: {BUFFER_CAPACITY}")
    print(f"Liczba producentow: {NUM_PRODUCERS}, Konsumentow: {NUM_CONSUMERS}")
    print(f"Liczba elementow na producenta: {ITEMS_PER_PRODUCER}")

    # Uzywamy MonitorPool, aby kazdy proces (producent/konsument) mial swoja instancje klienta monitora
    # ale wszystkie odwolują sie do tego samego logicznego monitora na serwerze.
    # W tym prostym przykladzie, kazdy proces tworzy wlasny monitor, co jest OK.
    # Dla bardziej zlozonych scenariuszy, gdzie watki w procesie wspoldziela monitor, MonitorPool jest lepszy.
    
    # Tworzenie instancji monitora i bufora - te beda wspoldzielone przez referencje
    # (ale kazdy proces bedzie mial wlasnego klienta monitora, jesli tak zaimplementujemy)
    # W tym przypadku, dla uproszczenia, kazdy proces tworzy wlasnego klienta,
    # ale odwoluje sie do tej samej nazwy monitora na serwerze.
    
    # Aby BoundedBuffer byl wspoldzielony, musimy go przekazac do procesow.
    # DistributedMonitor sam w sobie nie jest latwo "picklable" dla multiprocessing.
    # Zamiast tego kazdy proces potomny utworzy wlasnego klienta DistributedMonitor.
    # BoundedBuffer bedzie tworzony w kazdym procesie potomnym, uzywajac tego samego monitor_name.

    manager = Manager()
    produced_items = manager.list()
    consumed_items = manager.list()
    shared_actual_buffer = manager.list() # To bedzie nasz wspoldzielony bufor (jako lista)

    processes = []
    total_items_to_produce = NUM_PRODUCERS * ITEMS_PER_PRODUCER

    # Tworzenie i uruchamianie producentow
    for i in range(NUM_PRODUCERS):
        p = Process(target=producer_process, args=(
            i, 
            MONITOR_NAME_BB, 
            SERVER_ADDRESS, 
            BUFFER_CAPACITY, 
            shared_actual_buffer,
            ITEMS_PER_PRODUCER, 
            produced_items
        ))
        processes.append(p)
        p.start()

    # Tworzenie i uruchamianie konsumentow
    for i in range(NUM_CONSUMERS):
        # Kazdy konsument probuje pobrac swoja czesc wszystkich wyprodukowanych elementow
        items_for_this_consumer = total_items_to_produce // NUM_CONSUMERS
        if i < total_items_to_produce % NUM_CONSUMERS: # Rozdziel reszte
            items_for_this_consumer +=1
        
        p = Process(target=consumer_process, args=(
            i, 
            MONITOR_NAME_BB, 
            SERVER_ADDRESS, 
            BUFFER_CAPACITY, 
            shared_actual_buffer,
            items_for_this_consumer, 
            consumed_items
        ))
        processes.append(p)
        p.start()

    # Oczekiwanie na zakonczenie wszystkich procesow
    for p in processes:
        p.join()

    print("\nWszystkie procesy zakonczyly dzialanie.")
    print(f"Wyprodukowano ({len(produced_items)}): {sorted(list(produced_items))}")
    print(f"Skomsumowano ({len(consumed_items)}): {sorted(list(consumed_items))}")

    if sorted(list(produced_items)) == sorted(list(consumed_items)) and len(produced_items) == total_items_to_produce:
        print("\nTest Producent-Konsument ZAKONCZONY SUKCESEM: Wszystkie elementy zostaly poprawnie wyprodukowane i skonsumowane.")
    else:
        print("\nTest Producent-Konsument ZAKONCZONY BLEDEM.")
        if len(produced_items) != total_items_to_produce:
            print(f"  Blad: Oczekiwano {total_items_to_produce} wyprodukowanych elementow, otrzymano {len(produced_items)}.")
        if len(consumed_items) != total_items_to_produce:
            print(f"  Blad: Oczekiwano {total_items_to_produce} skonsumowanych elementow, otrzymano {len(consumed_items)}.")
        if sorted(list(produced_items)) != sorted(list(consumed_items)):
            print("  Blad: Lista wyprodukowanych i skonsumowanych elementow nie jest identyczna.")