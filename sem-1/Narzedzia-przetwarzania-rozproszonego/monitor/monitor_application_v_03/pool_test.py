"""
This script provides a simple demonstration of the `MonitorPool` class.

The `MonitorPool` is designed to optimize performance by creating only one
instance of a `DistributedMonitor` for a given monitor name and sharing that
single instance across all threads within a single process.

This test will show:
1. In the main thread, getting a monitor by the same name twice returns the
   exact same object.
2. Multiple threads asking for a monitor with the same name all receive a
   reference to that one single object.
"""

import time
from multiprocessing import Process
from threading import Thread, current_thread, Lock
import zmq

from monitor_server import MonitorServer
from monitor_client import MonitorPool

# --- 1. Configuration ---
SERVER_PORT = 5571
SERVER_ADDRESS = f"tcp://localhost:{SERVER_PORT}"

# --- 2. Helper for printing ---
def print_message(message):
    """Helper to print messages with a consistent format."""
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    thread_name = current_thread().name
    print(f"{timestamp} - {thread_name: <15} - {message}")

# --- 3. Server Process Target ---
def server_process_target(port):
    """This is the function the server process will execute."""
    # The server's own logging is not affected by our print functions.
    server = MonitorServer(port=port)
    server.start()

# --- 4. Thread Worker Target ---
def pool_user_thread(monitor_pool, results_list, lock):
    """A function that will be run by multiple threads."""
    print_message("Thread started, getting monitor 'multi_thread_monitor'.")
    
    # Each thread asks the pool for the monitor.
    monitor = monitor_pool.get_monitor("multi_thread_monitor")
    
    # We store the unique memory ID of the received monitor object.
    # If the pool works, all threads should get the same object, so all IDs will be the same.
    results_list.append(id(monitor))
    
    print_message(f"Got monitor object with id: {id(monitor)}.")

    # This lock is crucial for the test. The DistributedMonitor object is not
    # thread-safe (its internal state gets confused if used by multiple threads
    # at once). This lock ensures only one thread uses the object at a time.
    with lock:
        print_message("Acquired test lock, now using the shared monitor object.")
        # Use the monitor briefly to show it works.
        try:
            monitor.enter()
            print_message("Inside monitor critical section.")
            time.sleep(0.2)
        finally:
            monitor.exit()
            print_message("Exited monitor.")
    print_message("Released test lock.")

# --- 5. Main Execution Block ---
if __name__ == "__main__":
    print_message("--- Starting MonitorPool Demonstration ---")

    # --- Server Setup ---
    # A server must be running for the MonitorPool to create clients.
    server_proc = Process(target=server_process_target, args=(SERVER_PORT,), name="MonitorServer")
    server_proc.daemon = True
    server_proc.start()
    time.sleep(0.5)
    print_message("Monitor server process has been started.")

    # --- Pool Creation ---
    # We create a single pool instance for our application.
    pool = MonitorPool(SERVER_ADDRESS)
    print_message("MonitorPool created.")

    # --- Part 1: Demonstrate object identity in the main thread ---
    print_message("\n--- Part 1: Testing in Main Thread ---")
    print_message("Getting 'monitor_A' for the first time...")
    monitor_A1 = pool.get_monitor("monitor_A")
    print_message(f"Got monitor_A1 with id: {id(monitor_A1)}")

    print_message("Getting 'monitor_A' for the second time...")
    monitor_A2 = pool.get_monitor("monitor_A")
    print_message(f"Got monitor_A2 with id: {id(monitor_A2)}")

    # We use the `is` operator to check if they are the exact same object in memory.
    if monitor_A1 is monitor_A2:
        print_message("SUCCESS: monitor_A1 and monitor_A2 are the same object.")
    else:
        print_message("FAILURE: monitor_A1 and monitor_A2 are different objects.")

    print_message("Getting 'monitor_B' to show it's a different object...")
    monitor_B = pool.get_monitor("monitor_B")
    print_message(f"Got monitor_B with id: {id(monitor_B)}")
    if monitor_A1 is not monitor_B:
        print_message("SUCCESS: monitor_A1 and monitor_B are different objects.")
    else:
        print_message("FAILURE: monitor_A1 and monitor_B are the same object.")

    # --- Part 2: Demonstrate object sharing across multiple threads ---
    print_message("\n--- Part 2: Testing with Multiple Threads ---")
    threads = []
    thread_results = []
    num_threads = 3
    # We create a lock to serialize access to the non-thread-safe monitor object.
    test_lock = Lock()

    print_message(f"Starting {num_threads} threads...")
    for i in range(num_threads):
        # We pass the same pool object, the results list, and the lock to each thread.
        thread = Thread(target=pool_user_thread, args=(pool, thread_results, test_lock), name=f"PoolUserThread-{i}")
        threads.append(thread)
        thread.start()

    for thread in threads:
        thread.join()
    print_message("All threads have finished.")

    print_message(f"Object IDs received by threads: {thread_results}")
    # We check if all the IDs in the results list are the same.
    if len(set(thread_results)) == 1:
        print_message("SUCCESS: All threads received the exact same monitor object from the pool.")
    else:
        print_message("FAILURE: Threads received different monitor objects.")

    # --- Cleanup ---
    print_message("\n--- Cleanup ---")
    pool.close_all()
    print_message("MonitorPool closed.")

    # --- Server Teardown ---
    try:
        context = zmq.Context.instance()
        socket = context.socket(zmq.REQ)
        socket.setsockopt(zmq.LINGER, 0)
        socket.setsockopt(zmq.RCVTIMEO, 1000)
        socket.connect(SERVER_ADDRESS)
        socket.send_json({"action": "STOP_SERVER"})
        socket.recv_json()
        socket.close()
        context.term()
    except zmq.ZMQError as e:
        print_message(f"WARNING: Could not stop server cleanly: {e}")
    
    server_proc.join(timeout=2)
    if not server_proc.is_alive():
        print_message("Server process exited gracefully.")
    else:
        print_message("WARNING: Server did not exit gracefully. Forcing termination.")
        server_proc.terminate()

    print_message("--- MonitorPool Demonstration Finished ---")
