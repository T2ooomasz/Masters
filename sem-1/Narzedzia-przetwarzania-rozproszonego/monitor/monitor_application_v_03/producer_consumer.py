"""
Aplikacja testowa producent-konsument wykorzystująca BoundedBuffer
z rozproszonym monitorem.
"""
import time
import random
from multiprocessing import Process, current_process, Manager
import sys
import os

# Dodaj ścieżkę do katalogu nadrzędnego
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    from monitor_client import DistributedMonitor, MonitorPool
    from bounded_buffer import BoundedBuffer
except ImportError:
    print("Nie można zaimportować modułów. Upewnij się, że monitor_client.py i bounded_buffer.py są w PYTHONPATH lub w bieżącym katalogu.")
    sys.exit(1)

# Konfiguracja
SERVER_ADDRESS = "tcp://localhost:5555" # Upewnij się, że serwer monitora działa pod tym adresem
MONITOR_NAME_BB = "bounded_buffer_monitor_app"
BUFFER_CAPACITY = 5
NUM_PRODUCERS = 2
NUM_CONSUMERS = 2
ITEMS_PER_PRODUCER = 10

def producer_process(producer_id: int, buffer_proxy: BoundedBuffer, num_items: int, produced_items_list):
    """
    Proces producenta.
    """
    print(f"Producent {producer_id} (PID: {current_process().pid}, Monitor Client PID: {buffer_proxy.monitor.process_id}) startuje...")
    for i in range(num_items):
        item = f"Item-{producer_id}-{i}"
        time.sleep(random.uniform(0.1, 0.5)) # Symulacja produkcji
        print(f"Producent {producer_id} chce dodać: {item}")
        buffer_proxy.put(item)
        print(f"Producent {producer_id} dodał: {item}")
        produced_items_list.append(item)
    print(f"Producent {producer_id} zakończył.")

def consumer_process(consumer_id: int, buffer_proxy: BoundedBuffer, num_items_to_consume: int, consumed_items_list):
    """
    Proces konsumenta.
    """
    print(f"Konsument {consumer_id} (PID: {current_process().pid}, Monitor Client PID: {buffer_proxy.monitor.process_id}) startuje...")
    for _ in range(num_items_to_consume):
        time.sleep(random.uniform(0.2, 0.8)) # Symulacja konsumpcji
        print(f"Konsument {consumer_id} chce pobrać element...")
        item = buffer_proxy.get()
        print(f"Konsument {consumer_id} pobrał: {item}")
        consumed_items_list.append(item)
    print(f"Konsument {consumer_id} zakończył.")

if __name__ == "__main__":
    print("Uruchamianie aplikacji Producent-Konsument z rozproszonym monitorem...")
    print(f"Adres serwera monitora: {SERVER_ADDRESS}")
    print(f"Nazwa monitora dla BoundedBuffer: {MONITOR_NAME_BB}")
    print(f"Pojemność bufora: {BUFFER_CAPACITY}")
    print(f"Liczba producentów: {NUM_PRODUCERS}, Konsumentów: {NUM_CONSUMERS}")
    print(f"Liczba elementów na producenta: {ITEMS_PER_PRODUCER}")

    # Używamy MonitorPool, aby każdy proces (producent/konsument) miał swoją instancję klienta monitora
    # ale wszystkie odwołują się do tego samego logicznego monitora na serwerze.
    # W tym prostym przykładzie, każdy proces tworzy własny monitor, co jest OK.
    # Dla bardziej złożonych scenariuszy, gdzie wątki w procesie współdzielą monitor, MonitorPool jest lepszy.
    
    # Tworzenie instancji monitora i bufora - te będą współdzielone przez referencję
    # (ale każdy proces będzie miał własnego klienta monitora, jeśli tak zaimplementujemy)
    # W tym przypadku, dla uproszczenia, każdy proces tworzy własnego klienta,
    # ale odwołuje się do tej samej nazwy monitora na serwerze.
    
    # Aby BoundedBuffer był współdzielony, musimy go przekazać do procesów.
    # DistributedMonitor sam w sobie nie jest łatwo "picklable" dla multiprocessing.
    # Zamiast tego, każdy proces potomny utworzy własnego klienta DistributedMonitor.
    # BoundedBuffer będzie tworzony w każdym procesie potomnym, używając tego samego monitor_name.

    manager = Manager()
    produced_items = manager.list()
    consumed_items = manager.list()

    processes = []
    total_items_to_produce = NUM_PRODUCERS * ITEMS_PER_PRODUCER

    # Tworzenie i uruchamianie producentów
    for i in range(NUM_PRODUCERS):
        # Każdy proces tworzy własnego klienta monitora i BoundedBuffer
        # Ważne: używają tej samej nazwy monitora i adresu serwera
        monitor_client = DistributedMonitor(MONITOR_NAME_BB, SERVER_ADDRESS)
        bb_instance = BoundedBuffer(BUFFER_CAPACITY, monitor_client)
        p = Process(target=producer_process, args=(i, bb_instance, ITEMS_PER_PRODUCER, produced_items))
        processes.append(p)
        p.start()

    # Tworzenie i uruchamianie konsumentów
    for i in range(NUM_CONSUMERS):
        monitor_client = DistributedMonitor(MONITOR_NAME_BB, SERVER_ADDRESS)
        bb_instance = BoundedBuffer(BUFFER_CAPACITY, monitor_client)
        # Każdy konsument próbuje pobrać swoją część wszystkich wyprodukowanych elementów
        items_for_this_consumer = total_items_to_produce // NUM_CONSUMERS
        if i < total_items_to_produce % NUM_CONSUMERS: # Rozdziel resztę
            items_for_this_consumer +=1
        
        p = Process(target=consumer_process, args=(i, bb_instance, items_for_this_consumer, consumed_items))
        processes.append(p)
        p.start()

    # Oczekiwanie na zakończenie wszystkich procesów
    for p in processes:
        p.join()

    print("\nWszystkie procesy zakończyły działanie.")
    print(f"Wyprodukowano ({len(produced_items)}): {sorted(list(produced_items))}")
    print(f"Skomsumowano ({len(consumed_items)}): {sorted(list(consumed_items))}")

    if sorted(list(produced_items)) == sorted(list(consumed_items)) and len(produced_items) == total_items_to_produce:
        print("\n✅ Test Producent-Konsument ZAKOŃCZONY SUKCESEM: Wszystkie elementy zostały poprawnie wyprodukowane i skonsumowane.")
    else:
        print("\n❌ Test Producent-Konsument ZAKOŃCZONY BŁĘDEM.")
        if len(produced_items) != total_items_to_produce:
            print(f"  Błąd: Oczekiwano {total_items_to_produce} wyprodukowanych elementów, otrzymano {len(produced_items)}.")
        if len(consumed_items) != total_items_to_produce:
            print(f"  Błąd: Oczekiwano {total_items_to_produce} skonsumowanych elementów, otrzymano {len(consumed_items)}.")
        if sorted(list(produced_items)) != sorted(list(consumed_items)):
            print("  Błąd: Lista wyprodukowanych i skonsumowanych elementów nie jest identyczna.")

