import unittest
import time
import logging
from multiprocessing import Process, Barrier, Manager

# UWAGA: Upewnij się, że pliki monitor_server.py i monitor_client.py
# znajdują się w tym samym katalogu lub w ścieżce Pythona.
from monitor_server import MonitorServer
from monitor_client import DistributedMonitor

# Wyłączamy logowanie z modułów, aby nie zaśmiecać wyników testów
logging.disable(logging.CRITICAL)

def run_server(port):
    """Funkcja pomocnicza do uruchamiania serwera w osobnym procesie."""
    server = MonitorServer(port=port)
    server.start()

class BaseMonitorTestCase(unittest.TestCase):
    """
    Klasa bazowa, która automatycznie uruchamia i zatrzymuje serwer
    dla każdej klasy testowej.
    """
    SERVER_PORT = 5555
    SERVER_ADDRESS = f"tcp://localhost:{SERVER_PORT}"
    MONITOR_NAME = "test_monitor_global" # Domyślna nazwa monitora dla testów
    server_process = None

    @classmethod
    def setUpClass(cls):
        """Uruchom serwer raz, przed wszystkimi testami w klasie."""
        cls.server_process = Process(target=run_server, args=(cls.SERVER_PORT,))
        cls.server_process.start()
        time.sleep(0.5) # Daj serwerowi chwilę na start

    @classmethod
    def tearDownClass(cls):
        """Zatrzymaj serwer raz, po wszystkich testach w klasie."""
        cls.server_process.terminate()
        cls.server_process.join()

class TestBasicConnectivity(BaseMonitorTestCase):
    """Testy podstawowej łączności klient-serwer."""

    def test_01_client_connection(self):
        """Test: Klient może się połączyć z serwerem i rozłączyć."""
        print("\nTest: Klient może się połączyć z serwerem...")
        try:
            client = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
            # Połączenie jest nawiązywane przy pierwszej operacji
            client._connect()
            client.close()
            result = True
        except Exception:
            result = False
        self.assertTrue(result, "Klient nie mógł się połączyć z serwerem.")
        print("ok")

class TestMutexOperations(BaseMonitorTestCase):
    """Testy operacji na muteksie (enter/exit)."""

    def test_02_basic_enter_exit(self):
        """Test: Podstawowe wejście/wyjście z monitora."""
        print("\nTest: Podstawowe wejście/wyjście z monitora...")
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        with monitor:
            self.assertTrue(monitor.is_entered())
        self.assertFalse(monitor.is_entered())
        print("ok")

    def test_03_double_enter_error(self):
        """Test: Błąd przy próbie podwójnego wejścia do monitora."""
        print("\nTest: Błąd przy podwójnym wejściu...")
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        monitor.enter()
        with self.assertRaises(MonitorError): # Zmieniono z RuntimeError
            monitor.enter()
        monitor.exit()
        monitor.close()
        print("ok")

    def test_04_exit_without_enter_error(self):
        """Test: Błąd przy próbie wyjścia z monitora bez wejścia."""
        print("\nTest: Błąd przy wyjściu bez wejścia...")
        monitor = DistributedMonitor(self.MONITOR_NAME, self.SERVER_ADDRESS)
        with self.assertRaises(MonitorError): # Zmieniono z RuntimeError
            monitor.exit()
        monitor.close()
        print("ok")

class TestConcurrentAccess(BaseMonitorTestCase):
    """Testy wzajemnego wykluczania z wieloma klientami."""
    
    def test_05_mutual_exclusion(self):
        """Test: Wzajemne wykluczanie między wieloma procesami."""
        print("\nTest: Wzajemne wykluczanie między procesami...")
        
        num_processes = 3
        barrier = Barrier(num_processes)
        manager = Manager()
        # Lista do zapisywania przedziałów czasowych, w których procesy były w monitorze
        time_intervals = manager.list()

        # Używamy tej samej nazwy monitora dla wszystkich procesów w tym teście
        worker_monitor_name = f"{self.MONITOR_NAME}_mutex"
        def worker_process(monitor_name_arg):
            monitor = DistributedMonitor(monitor_name_arg, self.SERVER_ADDRESS)
            barrier.wait()
            with monitor:
                start_time = time.monotonic()
                time.sleep(0.2) # Symulacja pracy w sekcji krytycznej
                end_time = time.monotonic()
                time_intervals.append((start_time, end_time))

        processes = [Process(target=worker_process, args=(worker_monitor_name,)) for _ in range(num_processes)]
        for p in processes:
            p.start()
        for p in processes:
            p.join()
            
        # Sprawdzenie, czy żaden przedział czasowy się nie pokrywa
        for i in range(len(time_intervals)):
            for j in range(i + 1, len(time_intervals)):
                start1, end1 = time_intervals[i]
                start2, end2 = time_intervals[j]
                # Warunek pokrywania się: max(start1, start2) < min(end1, end2)
                self.assertFalse(max(start1, start2) < min(end1, end2), "Wykryto naruszenie wzajemnego wykluczania!")
        print("ok")


class TestConditionVariables(BaseMonitorTestCase):
    """Testy dla zmiennych warunkowych (wait, signal, broadcast)."""

    def test_06_basic_wait_signal(self):
        """Test: Jeden klient czeka (wait), drugi go budzi (signal)."""
        print("\nTest: Podstawowy wait/signal...")
        barrier = Barrier(2)
        condition_name = "data_ready_06"
        wait_signal_monitor_name = f"{self.MONITOR_NAME}_wait_signal"

        def waiter_process():
            with DistributedMonitor(wait_signal_monitor_name, self.SERVER_ADDRESS) as monitor:
                barrier.wait()
                monitor.wait(condition_name)

        def signaler_process():
            # Czekamy chwilę, aby 'waiter' na pewno wszedł pierwszy
            time.sleep(0.2)
            with DistributedMonitor(wait_signal_monitor_name, self.SERVER_ADDRESS) as monitor:
                barrier.wait()
                monitor.signal(condition_name)

        p_waiter = Process(target=waiter_process) # Nazwa monitora jest hardkodowana w funkcji
        p_signaler = Process(target=signaler_process)

        p_waiter.start()
        p_signaler.start()

        p_waiter.join(timeout=5)
        p_signaler.join(timeout=5)

        self.assertFalse(p_waiter.is_alive(), "Proces 'waiter' nie zakończył się.")
        print("ok")

    def test_07_broadcast(self):
        """Test: Jeden klient budzi wielu oczekujących (broadcast)."""
        print("\nTest: Sygnalizacja broadcast...")
        num_waiters = 3
        barrier = Barrier(num_waiters + 1)
        condition_name = "start_all_07"
        broadcast_monitor_name = f"{self.MONITOR_NAME}_broadcast"

        def waiter_process():
            with DistributedMonitor(broadcast_monitor_name, self.SERVER_ADDRESS) as monitor:
                barrier.wait()
                monitor.wait(condition_name)
        
        def broadcaster_process():
            # Czekamy, aż wszyscy waiterzy wejdą do kolejki
            time.sleep(0.5)
            with DistributedMonitor(broadcast_monitor_name, self.SERVER_ADDRESS) as monitor:
                barrier.wait()
                monitor.broadcast(condition_name)

        waiters = [Process(target=waiter_process) for _ in range(num_waiters)] # Nazwa monitora jest hardkodowana
        broadcaster = Process(target=broadcaster_process)

        for p in waiters:
            p.start()
        broadcaster.start()

        broadcaster.join(timeout=5)
        for p in waiters:
            p.join(timeout=5)

        self.assertFalse(broadcaster.is_alive(), "Proces 'broadcaster' nie zakończył się.")
        for i, p in enumerate(waiters):
            self.assertFalse(p.is_alive(), f"Proces 'waiter {i+1}' nie zakończył się.")
        print("ok")


if __name__ == '__main__':
    print("🧪 DISTRIBUTED MONITOR - TEST SUITE")
    print("============================================================")
    print("Testy zaprojektowane do precyzyjnej diagnostyki błędów")
    print("============================================================")
    unittest.main(verbosity=0)