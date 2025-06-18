"""
Distributed BoundedBuffer implementation using Monitor pattern with ØMQ
"""
import time
from typing import Any, Optional
from monitor_client import MonitorClient


class BoundedBuffer:
    """
    Thread-safe bounded buffer implementation using distributed monitor.
    Supports multiple producers and consumers across different processes.
    """
    
    def __init__(self, capacity: int, monitor_address: str = "tcp://localhost:5555"):
        self.capacity = capacity
        self.buffer = []
        self.monitor = MonitorClient(monitor_address)
        self.stats = {
            'puts': 0,
            'gets': 0,
            'put_waits': 0,
            'get_waits': 0
        }
    
    def put(self, item: Any, timeout: Optional[float] = None) -> bool:
        """
        Put item into buffer. Blocks if buffer is full.
        Returns True if successful, False if timeout occurred.
        """
        start_time = time.time()
        self.monitor.enter()
        
        try:
            # Wait while buffer is full
            wait_count = 0
            while self.is_full():
                if timeout and (time.time() - start_time) > timeout:
                    return False
                
                wait_count += 1
                self.monitor.wait("not_full")
            
            # Add item to buffer
            self.buffer.append(item)
            self.stats['puts'] += 1
            self.stats['put_waits'] += wait_count
            
            # Signal that buffer is not empty
            self.monitor.signal("not_empty")
            return True
            
        finally:
            self.monitor.exit()
    
    def get(self, timeout: Optional[float] = None) -> Optional[Any]:
        """
        Get item from buffer. Blocks if buffer is empty.
        Returns item or None if timeout occurred.
        """
        start_time = time.time()
        self.monitor.enter()
        
        try:
            # Wait while buffer is empty
            wait_count = 0
            while self.is_empty():
                if timeout and (time.time() - start_time) > timeout:
                    return None
                
                wait_count += 1
                self.monitor.wait("not_empty")
            
            # Remove item from buffer
            item = self.buffer.pop(0)
            self.stats['gets'] += 1
            self.stats['get_waits'] += wait_count
            
            # Signal that buffer is not full
            self.monitor.signal("not_full")
            return item
            
        finally:
            self.monitor.exit()
    
    def size(self) -> int:
        """Get current buffer size (non-blocking, approximate)"""
        return len(self.buffer)
    
    def is_full(self) -> bool:
        """Check if buffer is full"""
        return len(self.buffer) >= self.capacity
    
    def is_empty(self) -> bool:
        """Check if buffer is empty"""
        return len(self.buffer) == 0
    
    def get_stats(self) -> dict:
        """Get buffer statistics"""
        return self.stats.copy()
    
    def clear_stats(self):
        """Reset statistics"""
        self.stats = {
            'puts': 0,
            'gets': 0,
            'put_waits': 0,
            'get_waits': 0
        }


class Producer:
    """Producer that puts items into BoundedBuffer"""
    
    def __init__(self, producer_id: int, buffer: BoundedBuffer):
        self.id = producer_id
        self.buffer = buffer
        self.produced = 0
    
    def produce(self, count: int, delay: float = 0.1):
        """Produce specified number of items with optional delay"""
        for i in range(count):
            item = f"P{self.id}-Item{i}"
            
            if self.buffer.put(item):
                self.produced += 1
                print(f"Producer {self.id}: put {item}")
            else:
                print(f"Producer {self.id}: timeout putting {item}")
            
            if delay > 0:
                time.sleep(delay)


class Consumer:
    """Consumer that gets items from BoundedBuffer"""
    
    def __init__(self, consumer_id: int, buffer: BoundedBuffer):
        self.id = consumer_id
        self.buffer = buffer
        self.consumed = 0
    
    def consume(self, count: int, delay: float = 0.1):
        """Consume specified number of items with optional delay"""
        for i in range(count):
            item = self.buffer.get()
            
            if item is not None:
                self.consumed += 1
                print(f"Consumer {self.id}: got {item}")
            else:
                print(f"Consumer {self.id}: timeout getting item")
            
            if delay > 0:
                time.sleep(delay)


if __name__ == "__main__":
    # Simple test
    print("Testing BoundedBuffer...")
    
    buffer = BoundedBuffer(capacity=3)
    
    # Test basic functionality
    print("\n--- Basic Test ---")
    buffer.put("item1")
    buffer.put("item2")
    print(f"Buffer size: {buffer.size()}")
    
    item = buffer.get()
    print(f"Got: {item}")
    print(f"Buffer size: {buffer.size()}")
    
    print(f"Stats: {buffer.get_stats()}")