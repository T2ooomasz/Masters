"""
Implementacja BoundedBuffer (ograniczonego bufora) przy uzyciu
rozproszonego monitora.
"""
from collections import deque
import sys
import os

from monitor_client import DistributedMonitor, MonitorError
# Dodaj sciezke do katalogu nadrzednego, aby importowac monitor_client
# To jest potrzebne, jesli uruchamiasz ten plik bezposrednio,
# a monitor_client jest w katalogu o jeden poziom wyzej.
#sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))




class BoundedBuffer:
    """
    Ograniczony bufor implementujacy logike producent-konsument
    przy uzyciu rozproszonego monitora.
    """
    COND_NOT_FULL = "not_full"
    COND_NOT_EMPTY = "not_empty"

    # Zmieniamy __init__, aby przyjmowal wspoldzielona kolejke
    def __init__(self, capacity: int, monitor: DistributedMonitor, shared_deque_proxy):
        if capacity <= 0:
            raise ValueError("Pojemnosc bufora musi byc dodatnia")
        self.capacity = capacity
        self.monitor = monitor
        self.buffer_proxy = shared_deque_proxy # To bedzie np. manager.list() lub manager.Queue()

    def put(self, item):
        """
        Dodaje element do bufora. Blokuje, jesli bufor jest pelny.
        """
        with self.monitor: # Automatyczne enter() i exit()
            # Operujemy na buffer_proxy
            while len(self.buffer_proxy) >= self.capacity:
                print(f"Bufor pelny ({len(self.buffer_proxy)}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_FULL}'...")
                self.monitor.wait(self.COND_NOT_FULL)
            
            self.buffer_proxy.append(item) # Zakladajac, ze proxy ma metode append
            # print(f"Proces {self.monitor.process_id} dodal '{item}'. Bufor: {list(self.buffer_proxy)}")
            self.monitor.signal(self.COND_NOT_EMPTY)

    def get(self):
        """
        Pobiera element z bufora. Blokuje, jesli bufor jest pusty.
        """
        with self.monitor: # Automatyczne enter() i exit()
            # Operujemy na buffer_proxy
            while len(self.buffer_proxy) == 0:
                #print(f"Bufor pusty ({len(self.buffer_proxy)}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_EMPTY}'...")
                self.monitor.wait(self.COND_NOT_EMPTY)
            
            item = self.buffer_proxy.popleft() # Uzywamy popleft() dla deque (FIFO)
            # print(f"Proces {self.monitor.process_id} pobral '{item}'. Bufor: {list(self.buffer_proxy)}")
            self.monitor.signal(self.COND_NOT_FULL)
            return item

    def is_full(self) -> bool: # Pomocnicza, uzywana w logice wait
        # Ta metoda powinna byc wywolywana wewnatrz sekcji krytycznej monitora
        return len(self.buffer_proxy) >= self.capacity

    def is_empty(self) -> bool: # Pomocnicza, uzywana w logice wait
        # Ta metoda powinna byc wywolywana wewnatrz sekcji krytycznej monitora
        return len(self.buffer_proxy) == 0
