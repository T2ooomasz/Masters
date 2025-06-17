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

    def __init__(self, capacity: int, monitor: DistributedMonitor):
        if capacity <= 0:
            raise ValueError("Pojemność bufora musi być dodatnia")
        self.capacity = capacity
        self.monitor = monitor
        self.buffer = deque() # Używamy deque dla efektywnych operacji append i popleft

    def put(self, item):
        """
        Dodaje element do bufora. Blokuje, jeśli bufor jest pełny.
        """
        print(self.monitor.server_address)
        with self.monitor: # Automatyczne enter() i exit()
            while len(self.buffer) >= self.capacity:
                print(f"Bufor pełny ({len(self.buffer)}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_FULL}'...")
                self.monitor.wait(self.COND_NOT_FULL)
            
            self.buffer.append(item)
            # print(f"Proces {self.monitor.process_id} dodał '{item}'. Bufor: {list(self.buffer)}")
            self.monitor.signal(self.COND_NOT_EMPTY)

    def get(self):
        """
        Pobiera element z bufora. Blokuje, jeśli bufor jest pusty.
        """
        with self.monitor: # Automatyczne enter() i exit()
            while len(self.buffer) == 0:
                print(f"Bufor pusty. Proces {self.monitor.process_id} czeka na '{self.COND_NOT_EMPTY}'...")
                self.monitor.wait(self.COND_NOT_EMPTY)
            
            item = self.buffer.popleft() # FIFO
            # print(f"Proces {self.monitor.process_id} pobrał '{item}'. Bufor: {list(self.buffer)}")
            self.monitor.signal(self.COND_NOT_FULL)
            return item

    def is_full(self) -> bool: # Pomocnicza, używana w logice wait
        # Ta metoda powinna być wywoływana wewnątrz sekcji krytycznej monitora
        return len(self.buffer) >= self.capacity

    def is_empty(self) -> bool: # Pomocnicza, używana w logice wait
        # Ta metoda powinna być wywoływana wewnątrz sekcji krytycznej monitora
        return len(self.buffer) == 0