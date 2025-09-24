"""
Implementacja BoundedBuffer (ograniczonego bufora) przy uzyciu
rozproszonego monitora.
"""
from collections import deque
import sys
import os

from monitor_client import DistributedMonitor, MonitorError

class BoundedBuffer:
    """
    Ograniczony bufor implementujacy logike producent-konsument
    przy uzyciu rozproszonego monitora.
    """
    COND_NOT_FULL = "not_full"
    COND_NOT_EMPTY = "not_empty"

    def __init__(self, capacity: int, monitor: DistributedMonitor, shared_buffer_proxy):
        if capacity <= 0:
            raise ValueError("Pojemnosc bufora musi byc dodatnia")
        self.capacity = capacity
        self.monitor = monitor
        self.buffer_proxy = shared_buffer_proxy
        
        # Sprawdzamy, czy proxy jest do deque czy do listy
        # Manager.register() opakowuje oryginalne obiekty, wiec nie mozemy uzyc isinstance()
        # Zamiast tego, sprawdzamy obecnosc charakterystycznej metody.
        self._is_deque = hasattr(self.buffer_proxy, 'popleft')

    def _get_len(self):
        # AutoProxy nie ma __len__, wiec wywolujemy metode zdalnie
        return self.buffer_proxy._callmethod('__len__')

    def put(self, item):
        """
        Dodaje element do bufora. Blokuje, jesli bufor jest pelny.
        """
        with self.monitor:
            while self._get_len() >= self.capacity:
                print(f"Bufor pelny ({self._get_len()}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_FULL}'...")
                self.monitor.wait(self.COND_NOT_FULL)
            
            self.buffer_proxy.append(item)
            self.monitor.signal(self.COND_NOT_EMPTY)

    def get(self):
        """
        Pobiera element z bufora. Blokuje, jesli bufor jest pusty.
        """
        with self.monitor:
            while self._get_len() == 0:
                self.monitor.wait(self.COND_NOT_EMPTY)
            
            if self._is_deque:
                item = self.buffer_proxy.popleft()  # FIFO dla deque
            else:
                item = self.buffer_proxy.pop(0)  # FIFO dla listy
                
            self.monitor.signal(self.COND_NOT_FULL)
            return item

    def is_full(self) -> bool:
        return self._get_len() >= self.capacity

    def is_empty(self) -> bool:
        return self._get_len() == 0