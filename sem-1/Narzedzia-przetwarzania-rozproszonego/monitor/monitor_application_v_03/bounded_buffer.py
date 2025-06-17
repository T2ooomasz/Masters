"""
Implementacja BoundedBuffer (ograniczonego bufora) przy użyciu
rozproszonego monitora.
"""
from collections import deque
import sys
import os

from monitor_client import DistributedMonitor, MonitorError
# Dodaj ścieżkę do katalogu nadrzędnego, aby importować monitor_client
# To jest potrzebne, jeśli uruchamiasz ten plik bezpośrednio,
# a monitor_client jest w katalogu o jeden poziom wyżej.
#sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))




class BoundedBuffer:
    """
    Ograniczony bufor implementujący logikę producent-konsument
    przy użyciu rozproszonego monitora.
    """
    COND_NOT_FULL = "not_full"
    COND_NOT_EMPTY = "not_empty"

    # Zmieniamy __init__, aby przyjmował współdzieloną kolejkę
    def __init__(self, capacity: int, monitor: DistributedMonitor, shared_deque_proxy):
        if capacity <= 0:
            raise ValueError("Pojemność bufora musi być dodatnia")
        self.capacity = capacity
        self.monitor = monitor
        self.buffer_proxy = shared_deque_proxy # To będzie np. manager.list() lub manager.Queue()

    def put(self, item):
        """
        Dodaje element do bufora. Blokuje, jeśli bufor jest pełny.
        """
        with self.monitor: # Automatyczne enter() i exit()
            # Operujemy na buffer_proxy
            while len(self.buffer_proxy) >= self.capacity:
                print(f"Bufor pełny ({len(self.buffer_proxy)}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_FULL}'...")
                self.monitor.wait(self.COND_NOT_FULL)
            
            self.buffer_proxy.append(item) # Zakładając, że proxy ma metodę append
            # print(f"Proces {self.monitor.process_id} dodał '{item}'. Bufor: {list(self.buffer_proxy)}")
            self.monitor.signal(self.COND_NOT_EMPTY)

    def get(self):
        """
        Pobiera element z bufora. Blokuje, jeśli bufor jest pusty.
        """
        with self.monitor: # Automatyczne enter() i exit()
            # Operujemy na buffer_proxy
            while len(self.buffer_proxy) == 0:
                print(f"Bufor pusty ({len(self.buffer_proxy)}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_EMPTY}'...")
                self.monitor.wait(self.COND_NOT_EMPTY)
            
            item = self.buffer_proxy.pop(0) # Zakładając, że proxy ma metodę pop(0) dla FIFO
            # print(f"Proces {self.monitor.process_id} pobrał '{item}'. Bufor: {list(self.buffer_proxy)}")
            self.monitor.signal(self.COND_NOT_FULL)
            return item

    def is_full(self) -> bool: # Pomocnicza, używana w logice wait
        # Ta metoda powinna być wywoływana wewnątrz sekcji krytycznej monitora
        return len(self.buffer) >= self.capacity

    def is_empty(self) -> bool: # Pomocnicza, używana w logice wait
        # Ta metoda powinna być wywoływana wewnątrz sekcji krytycznej monitora
        return len(self.buffer) == 0