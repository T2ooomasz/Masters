import unittest
import time
import logging
from multiprocessing import Process, Barrier, Manager
import zmq

from monitor_server import MonitorServer
from monitor_client import DistributedMonitor, MonitorError

logging.disable(logging.CRITICAL)

def run_server(port):
    server = MonitorServer(port=port)
    server.start()

class BaseMonitorTestCase(unittest.TestCase):
    SERVER_PORT = 5555
    SERVER_ADDRESS = f"tcp://localhost:{SERVER_PORT}"
    MONITOR_NAME = "test_monitor_global"
    server_process = None

    @classmethod
    def setUpClass(cls):
        cls.server_process = Process(target=run_server, args=(cls.SERVER_PORT,))
        cls.server_process.daemon = True
        cls.server_process.start()
        time.sleep(0.5)

    @classmethod
    def tearDownClass(cls):
        # Łagodne zamknięcie serwera
        context = zmq.Context()
        socket = context.socket(zmq.REQ)
        socket.connect(cls.SERVER_ADDRESS)
        try:
            # Używamy teraz akcji "STOP"
            socket.send_json({"action": "STOP"})
            socket.recv_json()
        except zmq.ZMQError as e:
            logging.warning(f"Could not gracefully stop server: {e}")
        finally:
            socket.close()
            context.term()
        
        cls.server_process.join(timeout=2)
        if cls.server_process.is_alive():
            cls.server_process.terminate()
            cls.server_process.join()

class TestMutexOperations(BaseMonitorTestCase):
    def test_basic_enter_exit(self):
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        try:
            with monitor:
                self.assertTrue(monitor.is_entered())
            self.assertFalse(monitor.is_entered())
        finally:
            monitor.close()

    def test_double_enter_error(self):
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        try:
            monitor.enter()
            with self.assertRaises(MonitorError):
                monitor.enter()
            monitor.exit()
        finally:
            monitor.close()

    def test_exit_without_enter_error(self):
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        try:
            with self.assertRaises(MonitorError):
                monitor.exit()
        finally:
            monitor.close()

class TestConcurrentAccess(BaseMonitorTestCase):
    def test_mutual_exclusion(self):
        num_processes = 3
        barrier = Barrier(num_processes)
        manager = Manager()
        time_intervals = manager.list()
        worker_monitor_name = f"{self.MONITOR_NAME}_mutex"

        def worker_process(monitor_name_arg):
            monitor = None
            try:
                monitor = DistributedMonitor(monitor_name_arg, self.SERVER_ADDRESS)
                barrier.wait()
                with monitor:
                    start_time = time.monotonic()
                    time.sleep(0.2)
                    end_time = time.monotonic()
                    time_intervals.append((start_time, end_time))
            finally:
                if monitor:
                    monitor.close()

        processes = [Process(target=worker_process, args=(worker_monitor_name,)) for _ in range(num_processes)]
        for p in processes:
            p.start()
        for p in processes:
            p.join()
            
        for i in range(len(time_intervals)):
            for j in range(i + 1, len(time_intervals)):
                start1, end1 = time_intervals[i]
                start2, end2 = time_intervals[j]
                self.assertFalse(max(start1, start2) < min(end1, end2), "Wykryto naruszenie wzajemnego wykluczania!")

class TestConditionVariables(BaseMonitorTestCase):
    def test_basic_wait_signal(self):
        barrier = Barrier(2)
        condition_name = "data_ready"
        monitor_name = f"{self.MONITOR_NAME}_wait_signal"

        def waiter():
            monitor = None
            try:
                monitor = DistributedMonitor(monitor_name, self.SERVER_ADDRESS)
                with monitor:
                    barrier.wait()
                    monitor.wait(condition_name)
            finally:
                if monitor:
                    monitor.close()

        def signaler():
            monitor = None
            try:
                monitor = DistributedMonitor(monitor_name, self.SERVER_ADDRESS)
                barrier.wait()
                time.sleep(0.5)
                with monitor:
                    monitor.signal(condition_name)
            finally:
                if monitor:
                    monitor.close()

        p_waiter = Process(target=waiter)
        p_signaler = Process(target=signaler)
        p_waiter.start()
        p_signaler.start()
        p_waiter.join(timeout=5)
        p_signaler.join(timeout=5)
        self.assertFalse(p_waiter.is_alive())

if __name__ == '__main__':
    unittest.main()
