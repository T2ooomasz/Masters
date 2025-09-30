import time
import logging
from multiprocessing import Process, Value
import zmq

from monitor_server import MonitorServer
from monitor_client import DistributedMonitor

# --- Configuration ---
SERVER_PORT_BASE = 5560
SERVER_ADDRESS_TEMPLATE = "tcp://localhost:{port}"
MONITOR_NAME = "demo_monitor"

# --- Logging Setup ---
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(processName)-15s - %(levelname)-8s - %(message)s'
)

# --- Process Target Functions ---
def run_server(port):
    """Function to run the original monitor server."""
    server_logger = logging.getLogger()
    server_logger.setLevel(logging.WARNING)
    server = MonitorServer(port=port)
    server.start()

def mutex_worker(server_address, shared_counter):
    monitor = DistributedMonitor(MONITOR_NAME, server_address)
    logging.info("Attempting to enter monitor.")
    monitor.enter()
    logging.info("Inside monitor.")
    current_val = shared_counter.value
    time.sleep(0.5)
    shared_counter.value = current_val + 1
    logging.info("Exiting monitor.")
    monitor.exit()
    monitor.close()

def waiter_process(server_address):
    monitor = DistributedMonitor(MONITOR_NAME, server_address)
    condition_name = "data_ready"
    monitor.enter()
    logging.info("Inside monitor, will now wait on condition...")
    monitor.wait(condition_name)
    logging.info("Awakened from wait!")
    monitor.exit()
    monitor.close()

def signaler_process(server_address):
    monitor = DistributedMonitor(MONITOR_NAME, server_address)
    condition_name = "data_ready"
    logging.info("Entering monitor to signal.")
    monitor.enter()
    logging.info("Inside monitor, preparing to signal.")
    monitor.signal(condition_name)
    logging.info("Signal sent.")
    monitor.exit()
    monitor.close()

def broadcast_waiter(server_address):
    monitor = DistributedMonitor(MONITOR_NAME, server_address)
    condition_name = "start_work"
    monitor.enter()
    logging.info("Inside monitor, will now wait on condition.")
    monitor.wait(condition_name)
    logging.info("Awakened by broadcast!")
    monitor.exit()
    monitor.close()

def broadcaster_process(server_address):
    monitor = DistributedMonitor(MONITOR_NAME, server_address)
    condition_name = "start_work"
    logging.info("Entering monitor to broadcast.")
    monitor.enter()
    logging.info("Inside monitor, broadcasting...")
    monitor.broadcast(condition_name)
    logging.info("Broadcast sent.")
    monitor.exit()
    monitor.close()

# --- Test Functions ---
def test_mutual_exclusion():
    logging.info("--- Running Test 1: Mutual Exclusion ---")
    port = SERVER_PORT_BASE + 1
    server_address = SERVER_ADDRESS_TEMPLATE.format(port=port)
    num_processes = 3
    shared_counter = Value('i', 0)

    # 1. Start Server
    server_proc = Process(target=run_server, args=(port,), name=f"Server-p{port}")
    server_proc.daemon = True
    server_proc.start()
    time.sleep(0.5)

    # 2. Start and run client processes
    processes = [Process(target=mutex_worker, args=(server_address, shared_counter), name=f"MutexWorker-{i}") for i in range(num_processes)]
    for p in processes: p.start()
    for p in processes: p.join()

    # 3. Stop Server
    try:
        context = zmq.Context.instance()
        socket = context.socket(zmq.REQ)
        socket.connect(server_address)
        socket.send_json({"action": "STOP_SERVER"})
    finally:
        server_proc.terminate()

    logging.info(f"Final counter value: {shared_counter.value} (Expected: {num_processes})")
    assert shared_counter.value == num_processes
    logging.info("--- Test 1: Passed ---")

def test_wait_signal():
    logging.info("--- Running Test 2: Wait & Signal ---")
    port = SERVER_PORT_BASE + 2
    server_address = SERVER_ADDRESS_TEMPLATE.format(port=port)

    # 1. Start Server
    server_proc = Process(target=run_server, args=(port,), name=f"Server-p{port}")
    server_proc.daemon = True
    server_proc.start()
    time.sleep(0.5)

    # 2. Start and run client processes
    waiter = Process(target=waiter_process, args=(server_address,), name="Waiter")
    signaler = Process(target=signaler_process, args=(server_address,), name="Signaler")
    
    waiter.start()
    logging.info("Waiter started, giving it 1s to get into wait state...")
    time.sleep(1)

    logging.info("Starting signaler...")
    signaler.start()
    
    waiter.join()
    signaler.join()
        
    # 3. Stop Server
    try:
        context = zmq.Context.instance()
        socket = context.socket(zmq.REQ)
        socket.connect(server_address)
        socket.send_json({"action": "STOP_SERVER"})
    finally:
        server_proc.terminate()

    logging.info("--- Test 2: Passed ---")

def test_wait_broadcast():
    logging.info("--- Running Test 3: Wait & Broadcast ---")
    port = SERVER_PORT_BASE + 3
    server_address = SERVER_ADDRESS_TEMPLATE.format(port=port)
    num_waiters = 3

    # 1. Start Server
    server_proc = Process(target=run_server, args=(port,), name=f"Server-p{port}")
    server_proc.daemon = True
    server_proc.start()
    time.sleep(0.5)

    # 2. Start and run client processes
    waiters = [Process(target=broadcast_waiter, args=(server_address,), name=f"B-Waiter-{i}") for i in range(num_waiters)]
    broadcaster = Process(target=broadcaster_process, args=(server_address,), name="Broadcaster")
    
    for p in waiters: p.start()

    logging.info(f"Started {num_waiters} waiters, giving them 1.5s to get into wait state...")
    time.sleep(1.5)

    logging.info("Starting broadcaster...")
    broadcaster.start()
    
    all_procs = waiters + [broadcaster]
    for p in all_procs: p.join()

    # 3. Stop Server
    try:
        context = zmq.Context.instance()
        socket = context.socket(zmq.REQ)
        socket.connect(server_address)
        socket.send_json({"action": "STOP_SERVER"})
    finally:
        server_proc.terminate()

    logging.info("--- Test 3: Passed ---")

if __name__ == "__main__":
    test_mutual_exclusion()
    print("\n" + "="*80 + "\n")
    test_wait_signal()
    print("\n" + "="*80 + "\n")
    test_wait_broadcast()