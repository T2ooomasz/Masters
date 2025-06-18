"""
Testy wydajności i stresu dla BoundedBuffer
z rozproszonym monitorem.
"""

import time
import multiprocessing as mp
import sys
import os
import csv # Do zapisywania wyników

# Dodaj ścieżkę do katalogu nadrzędnego
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    from monitor_client import DistributedMonitor
    from bounded_buffer import BoundedBuffer
    # Importujemy również LocalBoundedBuffer, aby móc go użyć jako punkt odniesienia, jeśli chcemy
    # from local_vs_distributed import LocalBoundedBuffer # Załóżmy, że jest w tym samym katalogu lub odpowiednio zaimportowany
except ImportError:
    print("Nie można zaimportować modułów. Upewnij się, że monitor_client.py, bounded_buffer.py są w PYTHONPATH.")
    sys.exit(1)

# Funkcje workerów (producer_worker, consumer_worker) mogą być podobne do tych z local_vs_distributed.py
# Dla uproszczenia, skopiujemy je i dostosujemy, jeśli będzie potrzeba.

def producer_worker_perf(buffer_instance, num_items, producer_id, results_queue):
    """
    Worker producenta dla testów wydajności.
    Zbiera czasy operacji put.
    """
    latencies = []
    for i in range(num_items):
        item = (producer_id, i)
        start_op_time = time.perf_counter()
        buffer_instance.put(item)
        end_op_time = time.perf_counter()
        latencies.append(end_op_time - start_op_time)
    results_queue.put({"type": "producer", "id": producer_id, "latencies": latencies})

def consumer_worker_perf(buffer_instance, num_items, consumer_id, results_queue):
    """
    Worker konsumenta dla testów wydajności.
    Zbiera czasy operacji get.
    """
    latencies = []
    for _ in range(num_items):
        start_op_time = time.perf_counter()
        buffer_instance.get()
        end_op_time = time.perf_counter()
        latencies.append(end_op_time - start_op_time)
    results_queue.put({"type": "consumer", "id": consumer_id, "latencies": latencies})


def run_single_performance_test(num_producers, num_consumers, items_per_producer, capacity, server_address, monitor_name):
    """
    Uruchamia pojedynczy test wydajności dla danej konfiguracji.
    """
    print(f"\nUruchamianie testu: Producenci={num_producers}, Konsumenci={num_consumers}, Elementy/Prod={items_per_producer}, Pojemność={capacity}")

    manager = mp.Manager()
    shared_actual_buffer = manager.list()
    results_queue = manager.Queue() # Do zbierania wyników od workerów

    # Tworzenie instancji BoundedBuffer z DistributedMonitor
    # Każdy worker będzie tworzył swojego klienta monitora, ale BoundedBuffer potrzebuje jednego do inicjalizacji
    # (chociaż nie będzie on bezpośrednio używany przez workerów, jeśli tworzą własne instancje BoundedBuffer)
    # Lepsze podejście: workerzy tworzą pełne instancje BoundedBuffer

    processes = []
    total_items_to_produce = num_producers * items_per_producer

    start_total_time = time.perf_counter()

    # Producenci
    for i in range(num_producers):
        # Każdy worker tworzy własnego klienta monitora i instancję BoundedBuffer
        # przekazując mu współdzielony `shared_actual_buffer`
        # To jest zgodne z podejściem z `producer_consumer.py` i `local_vs_distributed.py`
        p = mp.Process(target=lambda: producer_worker_perf(
            BoundedBuffer(capacity, DistributedMonitor(monitor_name, server_address), shared_actual_buffer),
            items_per_producer,
            f"P{i}",
            results_queue
        ))
        processes.append(p)
        p.start()

    # Konsumenci
    items_per_consumer = total_items_to_produce // num_consumers
    extra_items = total_items_to_produce % num_consumers
    for i in range(num_consumers):
        items_to_consume = items_per_consumer + (1 if i < extra_items else 0)
        p = mp.Process(target=lambda: consumer_worker_perf(
            BoundedBuffer(capacity, DistributedMonitor(monitor_name, server_address), shared_actual_buffer),
            items_to_consume,
            f"C{i}",
            results_queue
        ))
        processes.append(p)
        p.start()

    for p in processes:
        p.join()

    end_total_time = time.perf_counter()
    total_duration = end_total_time - start_total_time
    operations_per_second = total_items_to_produce * 2 / total_duration # Każdy element to put i get

    # Zbieranie wyników latencji
    all_latencies = []
    while not results_queue.empty():
        result = results_queue.get()
        all_latencies.extend(result["latencies"])
    
    avg_latency = sum(all_latencies) / len(all_latencies) if all_latencies else 0

    print(f"Test zakończony. Czas całkowity: {total_duration:.4f} s")
    print(f"Operacje na sekundę (put+get): {operations_per_second:.2f}")
    print(f"Średnia latencja operacji: {avg_latency*1000:.4f} ms")

    return {
        "producers": num_producers,
        "consumers": num_consumers,
        "items_per_producer": items_per_producer,
        "capacity": capacity,
        "total_duration_s": total_duration,
        "ops_per_second": operations_per_second,
        "avg_latency_ms": avg_latency * 1000
    }

if __name__ == "__main__":
    server_address = "tcp://localhost:5555"
    base_monitor_name = "perf_stress_monitor"
    
    # Definicje scenariuszy testowych
    # (liczba producentów, liczba konsumentów, elementy na producenta, pojemność bufora)
    scenarios = [
        (1, 1, 100, 10),    # Bazowy
        (2, 2, 100, 10),    # Skalowalność - więcej P/C
        (4, 4, 100, 10),    # Skalowalność - jeszcze więcej P/C
        (2, 2, 500, 10),    # Stres - więcej elementów
        (2, 2, 100, 5),     # Stres - mniejszy bufor (więcej blokowania)
        (8, 8, 50, 20),     # Skalowalność - 8 P/C
        # Można dodać więcej scenariuszy
    ]

    all_results = []

    print("Rozpoczynanie testów wydajności i stresu dla rozproszonego monitora...")

    for i, (prods, cons, items, cap) in enumerate(scenarios):
        monitor_name = f"{base_monitor_name}_run{i}"
        # Oczyszczenie stanu na serwerze (jeśli serwer nie resetuje stanu dla nowych nazw monitorów)
        # W naszym przypadku, użycie nowej nazwy monitora powinno wystarczyć.
        # Alternatywnie, można by dodać komendę RESET do serwera.
        result = run_single_performance_test(prods, cons, items, cap, server_address, monitor_name)
        all_results.append(result)

    # Zapis wyników do pliku CSV
    output_filename = "performance_results_distributed.csv"
    print(f"\nZapisywanie wyników do {output_filename}...")
    if all_results:
        with open(output_filename, 'w', newline='') as csvfile:
            fieldnames = all_results[0].keys()
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writeheader()
            for row in all_results:
                writer.writerow(row)
        print(f"Wyniki zapisane.")
    else:
        print("Brak wyników do zapisania.")

    print("\nTesty wydajności zakończone.")
