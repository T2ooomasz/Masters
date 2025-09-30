"""
This script provides a very simple, educational demonstration of the `broadcast`
functionality in a distributed monitor system.

This version uses a simple time-based delay for synchronization instead of a
Barrier object, making the startup sequence very explicit and linear.

Scenario:
1. A central `MonitorServer` is started as a background process.
2. Multiple `waiter` processes are started.
3. The main script waits for a moment, assuming the waiters have now entered the
   monitor and are sleeping on a condition.
4. The `broadcaster` process is then started. It enters the monitor and issues
   a `broadcast` on the condition variable.
5. All waiting processes are awakened by the broadcast and proceed to finish.
6. The main script cleans up by shutting down the server process.
"""

import time
from multiprocessing import Process, current_process
import zmq

# We import the necessary components from the monitor implementation.
from monitor_server import MonitorServer
from monitor_client import DistributedMonitor

# --- 1. Configuration ---
SERVER_PORT = 5570
SERVER_ADDRESS = f"tcp://localhost:{SERVER_PORT}"
MONITOR_NAME = "broadcast_demo_monitor"
CONDITION_NAME = "start_work_condition"
NUM_WAITERS = 3

# --- 2. Helper for printing ---
def print_message(message):
    """Helper to print messages with a consistent format."""
    timestamp = time.strftime("%Y-%m-%d %H:%M:%S")
    process_name = current_process().name
    print(f"{timestamp} - {process_name: <15} - {message}")

# --- 3. Process Target Functions ---

def server_process_target(port):
    """This is the function the server process will execute."""
    server = MonitorServer(port=port)
    server.start()

def waiter_worker():
    """This worker process will wait to be awakened by a broadcast."""
    monitor = DistributedMonitor(MONITOR_NAME, SERVER_ADDRESS)
    
    # A process must own the monitor's lock before it can wait on a condition.
    monitor.enter()
    
    print_message("Inside monitor. Will now release the lock and wait on the condition.")
    
    # CRITICAL STEP: monitor.wait() does two things atomically:
    # 1. It releases the monitor lock, so other processes can enter.
    # 2. It puts this process to sleep until another process signals or broadcasts
    #    on this specific condition.
    monitor.wait(CONDITION_NAME)
    
    # When awakened, the process automatically re-acquires the lock before continuing.
    print_message("Awakened by broadcast! Re-acquired the lock.")
    
    # Now we can safely exit the monitor.
    monitor.exit()
    print_message("Exited monitor.")
    monitor.close()

def broadcaster_master():
    """This master process wakes up all waiting workers with a broadcast."""
    monitor = DistributedMonitor(MONITOR_NAME, SERVER_ADDRESS)
    
    print_message("Attempting to enter monitor to send the broadcast.")
    monitor.enter()
    
    print_message("Inside monitor. Sending broadcast now!")
    
    # CRITICAL STEP: monitor.broadcast() wakes up ALL processes waiting on this condition.
    woken_count = monitor.broadcast(CONDITION_NAME)
    print_message(f"Broadcast sent. Server reported {woken_count} process(es) awakened.")
    
    monitor.exit()
    print_message("Exited monitor after sending broadcast.")
    monitor.close()


# --- 4. Main Execution Block ---
if __name__ == "__main__":
    print_message("--- Starting Broadcast Demonstration ---")

    # --- Server Setup ---
    print_message(f"Starting monitor server in the background on port {SERVER_PORT}.")
    server_proc = Process(target=server_process_target, args=(SERVER_PORT,), name="MonitorServer")
    server_proc.daemon = True
    server_proc.start()
    time.sleep(0.5) # Give the server a moment to initialize.
    print_message("Monitor server process has been started.")

    # --- Test Processes Setup and Execution ---
    print_message(f"Starting {NUM_WAITERS} waiter processes.")

    # Create the process objects
    waiters = [Process(target=waiter_worker, name=f"Waiter-{i}") for i in range(NUM_WAITERS)]
    broadcaster = Process(target=broadcaster_master, name="Broadcaster")
    
    # Start only the waiter processes first.
    for p in waiters:
        p.start()
    
    # EDUCATIONAL NOTE: We are now using a time-based delay for synchronization.
    # We assume that 1 second is enough time for the OS to schedule all the
    # waiter processes and for them to enter the `monitor.wait()` state.
    # This is simpler to read than a Barrier, but less reliable.
    print_message("All waiters started. Giving them 1s to get into wait state...")
    time.sleep(1)

    # Now, we start the broadcaster process.
    print_message("Waking up (starting) the broadcaster process now...")
    broadcaster.start()
    
    # Join all processes to wait for them to complete.
    all_processes = waiters + [broadcaster]
    for p in all_processes:
        p.join()
    
    print_message("All worker processes have finished.")

    # --- Server Teardown ---
    print_message("Test finished. Stopping the monitor server.")
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
        print_message(f"WARNING: Could not send stop signal cleanly: {e}")
    
    server_proc.join(timeout=2)
    if server_proc.is_alive():
        print_message("WARNING: Server did not exit gracefully. Forcing termination.")
        server_proc.terminate()
        server_proc.join()
    else:
        print_message("Server process exited gracefully.")

    print_message("--- Broadcast Demonstration Finished ---")