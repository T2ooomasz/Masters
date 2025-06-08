"""
Performance comparison: Local Monitor vs Distributed Monitor
"""
import threading
import multiprocessing as mp
import time
import queue
from typing import List
from bounded_buffer import BoundedBuffer


class LocalBoundedBuffer:
    """Local implementation using threading primitives for comparison"""
    
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.buffer = []
        self.lock = threading.Lock()
        self.not_full = threading.Condition(self.lock)
        self.not_empty = threading.Condition(self.lock)
        self.stats = {'puts': 0, 'gets': 0, 'put_waits': 0, 'get_waits': 0}
    
    def put(self, item):
        with self.not_full:
            wait_count = 0
            while len(self.buffer) >= self.capacity:
                wait_count += 1
                self.not_full.wait()
            
            self.buffer.append(item)
            self.stats['puts'] += 1
            self.stats['put_waits'] += wait_count
            self.not_empty.notify()
    
    def get(self):
        with self.not_empty:
            wait_count = 0
            while len(self.buffer) == 0:
                wait_count += 1
                self.not_empty.wait()
            
            item = self.buffer.pop(0)
            self.stats['gets'] += 1
            self.stats['get_waits'] += wait_count
            self.not_full.notify()
            return item
    
    def get_stats(self):
        return self.stats.copy()


def local_producer_consumer_test(num_threads: int, items_per_thread: int, capacity: int):
    """Test with local threading implementation"""
    print(f"\n--- Local Threading Test ---")
    print(f"Threads: {num_threads}, Items: {items_per_thread}, Capacity: {capacity}")
    
    buffer = LocalBoundedBuffer(capacity)
    threads = []
    results = []
    
    def producer_worker(thread_id: int):
        start_time = time.time()
        for i in range(items_per_thread):
            buffer.put(f"T{thread_id}-Item{i}")
        end_time = time.time()
        results.append(('producer', thread_id, end_time - start_time))
    
    def consumer_worker(thread_id: int):
        start_time = time.time()
        for i in range(items_per_thread):
            item = buffer.get()
        end_time = time.time()
        results.append(('consumer', thread_id, end_time - start_time))
    
    # Start threads
    start_total = time.time()
    
    # Start producers
    for i in range(num_threads):
        t = threading.Thread(target=producer_worker, args=(i,))
        t.start()
        threads.append(t)
    
    # Start consumers
    for i in range(num_threads):
        t = threading.Thread(target=consumer_worker, args=(i,))
        t.start()
        threads.append(t)
    
    # Wait for completion
    for t in threads:
        t.join()
    
    end_total = time.time()
    
    # Calculate metrics
    total_ops = num_threads * items_per_thread * 2  # puts + gets
    total_time = end_total - start_total
    ops_per_sec = total_ops / total_time
    avg_latency = (total_time / total_ops) * 1000  # ms
    
    stats = buffer.get_stats()
    
    print(f"Total time: {total_time:.3f}s")
    print(f"Operations: {total_ops}")
    print(f"Throughput: {ops_per_sec:.1f} ops/sec")
    print(f"Avg latency: {avg_latency:.3f} ms/op")
    print(f"Wait events: puts={stats['put_waits']}, gets={stats['get_waits']}")
    
    return {
        'type': 'local',
        'total_time': total_time,
        'ops_per_sec': ops_per_sec,
        'avg_latency': avg_latency,
        'stats': stats
    }


def distributed_producer_worker(proc_id: int, items: int, capacity: int, 
                              monitor_address: str, result_queue: mp.Queue):
    """Distributed producer worker"""
    try:
        buffer = BoundedBuffer(capacity, monitor_address)
        start_time = time.time()
        
        for i in range(items):
            buffer.put(f"P{proc_id}-Item{i}")
        
        end_time = time.time()
        result_queue.put(('producer', proc_id, end_time - start_time))
        
    except Exception as e:
        print(f"Producer {proc_id} error: {e}")


def distributed_consumer_worker(proc_id: int, items: int, capacity: int,
                              monitor_address: str, result_queue: mp.Queue):
    """Distributed consumer worker"""
    try:
        buffer = BoundedBuffer(capacity, monitor_address)
        start_time = time.time()
        
        for i in range(items):
            item = buffer.get()
        
        end_time = time.time()
        result_queue.put(('consumer', proc_id, end_time - start_time))
        
    except Exception as e:
        print(f"Consumer {proc_id} error: {e}")


def distributed_producer_consumer_test(num_processes: int, items_per_process: int, 
                                     capacity: int, monitor_address: str):
    """Test with distributed implementation"""
    print(f"\n--- Distributed Processes Test ---")
    print(f"Processes: {num_processes}, Items: {items_per_process}, Capacity: {capacity}")
    
    result_queue = mp.Queue()
    processes = []
    
    start_total = time.time()
    
    # Start producer processes
    for i in range(num_processes):
        proc = mp.Process(
            target=distributed_producer_worker,
            args=(i, items_per_process, capacity, monitor_address, result_queue)
        )
        proc.start()
        processes.append(proc)
    
    # Start consumer processes  
    for i in range(num_processes):
        proc = mp.Process(
            target=distributed_consumer_worker,
            args=(i, items_per_process, capacity, monitor_address, result_queue)
        )
        proc.start()
        processes.append(proc)
    
    # Wait for completion
    for proc in processes:
        proc.join()
    
    end_total = time.time()
    
    # Collect results
    results = []
    while not result_queue.empty():
        results.append(result_queue.get())
    
    # Calculate metrics
    total_ops = num_processes * items_per_process * 2
    total_time = end_total - start_total
    ops_per_sec = total_ops / total_time if total_time > 0 else 0
    avg_latency = (total_time / total_ops) * 1000 if total_ops > 0 else 0
    
    print(f"Total time: {total_time:.3f}s")
    print(f"Operations: {total_ops}")
    print(f"Throughput: {ops_per_sec:.1f} ops/sec")
    print(f"Avg latency: {avg_latency:.3f} ms/op")
    
    return {
        'type': 'distributed',
        'total_time': total_time,
        'ops_per_sec': ops_per_sec,
        'avg_latency': avg_latency,
        'results': results
    }


def run_comparison(num_workers: int = 4, items_per_worker: int = 100, 
                  capacity: int = 10, monitor_address: str = "tcp://localhost:5555"):
    """Run performance comparison between local and distributed implementations"""
    
    print("=" * 60)
    print("PERFORMANCE COMPARISON: Local vs Distributed Monitor")
    print("=" * 60)
    print(f"Configuration: {num_workers} workers, {items_per_worker} items each, buffer capacity {capacity}")
    
    # Run local test
    local_result = local_producer_consumer_test(num_workers, items_per_worker, capacity)
    
    # Run distributed test
    distributed_result = distributed_producer_consumer_test(
        num_workers, items_per_worker, capacity, monitor_address
    )
    
    # Compare results
    print(f"\n--- COMPARISON ---")
    print(f"Local Threading:")
    print(f"  Time: {local_result['total_time']:.3f}s")
    print(f"  Throughput: {local_result['ops_per_sec']:.1f} ops/sec")
    print(f"  Latency: {local_result['avg_latency']:.3f} ms/op")
    
    print(f"Distributed Processes:")
    print(f"  Time: {distributed_result['total_time']:.3f}s")
    print(f"  Throughput: {distributed_result['ops_per_sec']:.1f} ops/sec") 
    print(f"  Latency: {distributed_result['avg_latency']:.3f} ms/op")
    
    if local_result['ops_per_sec'] > 0 and distributed_result['ops_per_sec'] > 0:
        slowdown = local_result['ops_per_sec'] / distributed_result['ops_per_sec']
        print(f"\nDistributed slowdown factor: {slowdown:.2f}x")
        
        overhead = distributed_result['avg_latency'] - local_result['avg_latency']
        print(f"Network overhead: {overhead:.3f} ms/op")
    
    return local_result, distributed_result


if __name__ == "__main__":
    print("Make sure monitor_server_step3.py is running on localhost:5555!")
    time.sleep(1)
    
    # Run comparison with different configurations
    configurations = [
        (2, 50, 5),   # Small test
        (4, 100, 10), # Medium test  
        (6, 200, 15), # Larger test
    ]
    
    for workers, items, capacity in configurations:
        try:
            run_comparison(workers, items, capacity)
            print("\n" + "="*60 + "\n")
        except Exception as e:
            print(f"Test failed: {e}")