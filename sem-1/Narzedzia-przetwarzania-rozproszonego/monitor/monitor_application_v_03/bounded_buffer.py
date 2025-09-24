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

    def __init__(self, capacity: int, monitor: DistributedMonitor, shared_buffer):
        if capacity <= 0:
            raise ValueError("Pojemnosc bufora musi byc dodatnia")
        self.capacity = capacity
        self.monitor = monitor
        self.buffer = shared_buffer # Renamed to 'buffer' for clarity, as it might not always be a proxy
        
        # Determine if the shared_buffer is a deque or a list, and if it's a proxy
        self._is_proxy = hasattr(self.buffer, '_callmethod')
        if self._is_proxy:
            # If it's a proxy, check the underlying type via its methods
            self._is_deque = hasattr(self.buffer, 'popleft')
        else:
            # If it's a direct object, check its type directly
            self._is_deque = isinstance(self.buffer, deque)

    def _get_len(self):
        if self._is_proxy:
            return self.buffer._callmethod('__len__')
        else:
            return len(self.buffer)

    def put(self, item):
        """
        Dodaje element do bufora. Blokuje, jesli bufor jest pelny.
        """
        with self.monitor:
            while self._get_len() >= self.capacity:
                print(f"Bufor pelny ({self._get_len()}/{self.capacity}). Proces {self.monitor.process_id} czeka na '{self.COND_NOT_FULL}'...")
                self.monitor.wait(self.COND_NOT_FULL)
            
            self.buffer.append(item)
            self.monitor.signal(self.COND_NOT_EMPTY)

    def get(self):
        """
        Pobiera element z bufora. Blokuje, jesli bufor jest pusty.
        """
        with self.monitor:
            while self._get_len() == 0:
                self.monitor.wait(self.COND_NOT_EMPTY)
            
            if self._is_deque:
                item = self.buffer.popleft()  # FIFO dla deque
            else:
                item = self.buffer.pop(0)  # FIFO dla listy
                
            self.monitor.signal(self.COND_NOT_FULL)
            return item

    def is_full(self) -> bool:
        return self._get_len() >= self.capacity

    def is_empty(self) -> bool:
        return self._get_len() == 0
