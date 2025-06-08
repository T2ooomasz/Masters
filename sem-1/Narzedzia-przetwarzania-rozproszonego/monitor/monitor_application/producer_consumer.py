"""
Multi-process Producer-Consumer application using Distributed BoundedBuffer
"""
import argparse
import multiprocessing as mp
import time
import signal
import sys
from bounded_buffer import BoundedBuffer, Producer, Consumer


def producer_process(producer_id: int, num_items: int, buffer_capacity: int, 
                    delay: float, monitor_address: str):
    """Producer process function"""
    try:
        print(f"Producer {producer_id} starting...")
        buffer = BoundedBuffer(buffer_capacity, monitor_address)
        producer = Producer(producer_id, buffer)
        
        start_time = time.time()
        producer.produce(num_items, delay)
        end_time = time.time()
        
        print(f"Producer {producer_id} finished: {producer.produced} items in {end_time - start_time:.2f}s")
        return producer.produced
        
    except KeyboardInterrupt:
        print(f"Producer {producer_id} interrupted")
    except Exception as e:
        print(f"Producer {producer_id} error: {e}")


def consumer_process(consumer_id: int, num_items: int, buffer_capacity: int,
                    delay: float, monitor_address: str):
    """Consumer process function"""
    try:
        print(f"Consumer {consumer_id} starting...")
        buffer = BoundedBuffer(buffer_capacity, monitor_address)
        consumer = Consumer(consumer_id, buffer)
        
        start_time = time.time()
        consumer.consume(num_items, delay)
        end_time = time.time()
        
        print(f"Consumer {consumer_id} finished: {consumer.consumed} items in {end_time - start_time:.2f}s")
        return consumer.consumed
        
    except KeyboardInterrupt:
        print(f"Consumer {consumer_id} interrupted")
    except Exception as e:
        print(f"Consumer {consumer_id} error: {e}")


def monitor_buffer(buffer_capacity: int, monitor_address: str, duration: int):
    """Monitor buffer state during test"""
    try:
        buffer = BoundedBuffer(buffer_capacity, monitor_address)
        
        print(f"Monitoring buffer for {duration} seconds...")
        start_time = time.time()
        
        while time.time() - start_time < duration:
            stats = buffer.get_stats()
            size = buffer.size()
            
            print(f"Buffer: size={size}/{buffer_capacity}, "
                  f"puts={stats['puts']}, gets={stats['gets']}, "
                  f"put_waits={stats['put_waits']}, get_waits={stats['get_waits']}")
            
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("Monitor interrupted")
    except Exception as e:
        print(f"Monitor error: {e}")


def run_test(num_producers: int, num_consumers: int, items_per_process: int,
             buffer_capacity: int, producer_delay: float, consumer_delay: float,
             monitor_address: str = "tcp://localhost:5555"):
    """Run complete producer-consumer test"""
    
    print(f"\n=== Producer-Consumer Test ===")
    print(f"Producers: {num_producers}, Consumers: {num_consumers}")
    print(f"Items per process: {items_per_process}")
    print(f"Buffer capacity: {buffer_capacity}")
    print(f"Producer delay: {producer_delay}s, Consumer delay: {consumer_delay}s")
    print(f"Monitor address: {monitor_address}")
    print("=" * 50)
    
    processes = []
    
    # Start monitor process
    monitor_proc = mp.Process(
        target=monitor_buffer,
        args=(buffer_capacity, monitor_address, items_per_process * 2)
    )
    monitor_proc.start()
    processes.append(monitor_proc)
    
    # Start producer processes  
    for i in range(num_producers):
        proc = mp.Process(
            target=producer_process,
            args=(i, items_per_process, buffer_capacity, producer_delay, monitor_address)
        )
        proc.start()
        processes.append(proc)
    
    # Start consumer processes
    for i in range(num_consumers):
        proc = mp.Process(
            target=consumer_process,
            args=(i, items_per_process, buffer_capacity, consumer_delay, monitor_address)
        )
        proc.start()
        processes.append(proc)
    
    # Handle Ctrl+C gracefully
    def signal_handler(sig, frame):
        print("\nShutting down processes...")
        for proc in processes:
            if proc.is_alive():
                proc.terminate()
        sys.exit(0)
    
    signal.signal(signal.SIGINT, signal_handler)
    
    # Wait for all processes to complete
    try:
        for proc in processes:
            proc.join()
        print("\nAll processes completed successfully!")
        
    except KeyboardInterrupt:
        print("\nTest interrupted by user")
        for proc in processes:
            if proc.is_alive():
                proc.terminate()


def main():
    parser = argparse.ArgumentParser(description="Distributed Producer-Consumer Test")
    parser.add_argument("--producers", type=int, default=2, help="Number of producer processes")
    parser.add_argument("--consumers", type=int, default=2, help="Number of consumer processes")
    parser.add_argument("--items", type=int, default=10, help="Items per process")
    parser.add_argument("--capacity", type=int, default=5, help="Buffer capacity")
    parser.add_argument("--prod-delay", type=float, default=0.2, help="Producer delay (seconds)")
    parser.add_argument("--cons-delay", type=float, default=0.3, help="Consumer delay (seconds)")
    parser.add_argument("--address", type=str, default="tcp://localhost:5555", help="Monitor server address")
    
    args = parser.parse_args()
    
    print("Starting Distributed Producer-Consumer Test")
    print("Make sure monitor_server_step3.py is running!")
    
    run_test(
        num_producers=args.producers,
        num_consumers=args.consumers,
        items_per_process=args.items,
        buffer_capacity=args.capacity,
        producer_delay=args.prod_delay,
        consumer_delay=args.cons_delay,
        monitor_address=args.address
    )


if __name__ == "__main__":
    main()