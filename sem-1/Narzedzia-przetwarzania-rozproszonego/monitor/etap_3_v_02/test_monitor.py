======================================================================
FAIL: test_06_basic_wait_signal (__main__.TestConditionVariables.test_06_basic_wait_signal)
Test: Jeden klient czeka (wait), drugi go budzi (signal).
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/home/tomek/repos/Masters/sem-1/Narzedzia-przetwarzania-rozproszonego/monitor/etap_3_v_02/test_monitor.py", line 162, in test_06_basic_wait_signal
    self.assertFalse(p_waiter.is_alive(), "Proces 'waiter' nie zakończył się.")
AssertionError: True is not false : Proces 'waiter' nie zakończył się.

======================================================================
FAIL: test_07_broadcast (__main__.TestConditionVariables.test_07_broadcast)
Test: Jeden klient budzi wielu oczekujących (broadcast).
----------------------------------------------------------------------
Traceback (most recent call last):
  File "/home/tomek/repos/Masters/sem-1/Narzedzia-przetwarzania-rozproszonego/monitor/etap_3_v_02/test_monitor.py", line 196, in test_07_broadcast
    self.assertFalse(broadcaster.is_alive(), "Proces 'broadcaster' nie zakończył się.")
AssertionError: True is not false : Proces 'broadcaster' nie zakończył się.

import unittest
import time
import logging
from multiprocessing import Process, Barrier, Manager

# UWAGA: Upewnij się, że pliki monitor_server.py i monitor_client.py
# znajdują się w tym samym katalogu lub w ścieżce Pythona.
from monitor_server import MonitorServer
from monitor_client import DistributedMonitor, MonitorError

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

    def test_08_selective_signaling_multiple_conditions(self):
        """Test: Selektywne budzenie procesów na różnych warunkach."""
        print("\nTest: Selektywne budzenie na wielu warunkach...")

        NUM_WAITERS_PER_CONDITION = 2
        TOTAL_WAITERS = NUM_WAITERS_PER_CONDITION * 2
        NUM_SIGNALERS = 2
        
        monitor_name_test = f"{self.MONITOR_NAME}_multi_cond_08"
        condition_X_name = "cond_X_08"
        condition_Y_name = "cond_Y_08"

        manager = Manager()
        completed_waiter_ids = manager.list()
        signaler_reports = manager.list() # (signaler_id, woken_count)
        # event_log = manager.list() # Opcjonalne, do debugowania

        # Bariera dla wszystkich procesów biorących udział w teście
        barrier = Barrier(TOTAL_WAITERS + NUM_SIGNALERS)

        waiter_processes = []
        waiter_ids_cond_X = [f"Waiter_X{i+1}" for i in range(NUM_WAITERS_PER_CONDITION)]
        waiter_ids_cond_Y = [f"Waiter_Y{i+1}" for i in range(NUM_WAITERS_PER_CONDITION)]

        def waiter_func(process_id_str, cond_name_to_wait, mon_name, srv_addr, barr, completed_list_proxy):
            # print(f"{process_id_str}: Uruchamiam...")
            monitor = DistributedMonitor(mon_name, srv_addr, poll_interval=0.05) # Krótszy poll dla testu
            try:
                barr.wait()
                # print(f"{process_id_str}: Po barierze, wchodzę do monitora...")
                with monitor:
                    # print(f"{process_id_str}: W monitorze, czekam na {cond_name_to_wait}...")
                    monitor.wait(cond_name_to_wait)
                    # print(f"{process_id_str}: Obudzony z {cond_name_to_wait}, wykonuję pracę...")
                    completed_list_proxy.append(process_id_str)
                    time.sleep(0.1) # Symulacja pracy w sekcji krytycznej
                    # print(f"{process_id_str}: Kończę pracę, wychodzę z monitora.")
            except MonitorError as e:
                print(f"Błąd w {process_id_str}: {e}") # Logowanie błędów z procesów potomnych
            except Exception as e:
                print(f"Nieoczekiwany błąd w {process_id_str}: {e}")
            finally:
                monitor.close()
                # print(f"{process_id_str}: Zakończono.")


        def signaler_func(signaler_id_str, cond_name_to_signal, mon_name, srv_addr, barr, reports_list_proxy, delay):
            # print(f"{signaler_id_str}: Uruchamiam...")
            monitor = DistributedMonitor(mon_name, srv_addr, poll_interval=0.05)
            try:
                barr.wait()
                # print(f"{signaler_id_str}: Po barierze, czekam {delay}s przed sygnałem...")
                time.sleep(delay) # Opóźnienie, aby kelnerzy zdążyli wejść w wait
                # print(f"{signaler_id_str}: Wchodzę do monitora, aby zasygnalizować {cond_name_to_signal}...")
                with monitor:
                    # print(f"{signaler_id_str}: W monitorze, sygnalizuję {cond_name_to_signal}...")
                    woken_count = monitor.signal(cond_name_to_signal)
                    reports_list_proxy.append((signaler_id_str, woken_count))
                    # print(f"{signaler_id_str}: Zasygnalizowano {cond_name_to_signal}, obudzono: {woken_count}. Wychodzę.")
            except MonitorError as e:
                print(f"Błąd w {signaler_id_str}: {e}")
            except Exception as e:
                print(f"Nieoczekiwany błąd w {signaler_id_str}: {e}")
            finally:
                monitor.close()
                # print(f"{signaler_id_str}: Zakończono.")

        # Tworzenie procesów kelnerów dla warunku X
        for pid_str in waiter_ids_cond_X:
            p = Process(target=waiter_func, args=(pid_str, condition_X_name, monitor_name_test, self.SERVER_ADDRESS, barrier, completed_waiter_ids))
            waiter_processes.append(p)

        # Tworzenie procesów kelnerów dla warunku Y
        for pid_str in waiter_ids_cond_Y:
            p = Process(target=waiter_func, args=(pid_str, condition_Y_name, monitor_name_test, self.SERVER_ADDRESS, barrier, completed_waiter_ids))
            waiter_processes.append(p)
        
        # Tworzenie procesów sygnalizatorów
        signaler_X = Process(target=signaler_func, args=("Signaler_X", condition_X_name, monitor_name_test, self.SERVER_ADDRESS, barrier, signaler_reports, 0.3))
        signaler_Y = Process(target=signaler_func, args=("Signaler_Y", condition_Y_name, monitor_name_test, self.SERVER_ADDRESS, barrier, signaler_reports, 0.5))

        all_processes = waiter_processes + [signaler_X, signaler_Y]
        for p in all_processes:
            p.start()

        # Oczekiwanie na zakończenie sygnalizatorów
        signaler_X.join(timeout=5)
        signaler_Y.join(timeout=5)
        self.assertFalse(signaler_X.is_alive(), "Signaler_X nie zakończył się.")
        self.assertFalse(signaler_Y.is_alive(), "Signaler_Y nie zakończył się.")

        # Sprawdzenie raportów sygnalizatorów
        self.assertEqual(len(signaler_reports), NUM_SIGNALERS, "Nie wszystkie raporty sygnalizatorów zostały zebrane.")
        for sid, count in signaler_reports:
            self.assertEqual(count, 1, f"Sygnalizator {sid} obudził {count} procesów, oczekiwano 1.")

        # Oczekiwanie na zakończenie procesów kelnerów (niektóre powinny się zakończyć, inne nie)
        time.sleep(1) # Dodatkowy czas na przetworzenie sygnałów i zakończenie pracy przez obudzonych kelnerów

        finished_waiters_count = 0
        for p in waiter_processes:
            p.join(timeout=0.1) 
            if not p.is_alive():
                finished_waiters_count += 1
            else:
                p.terminate() 
                p.join(timeout=1)

        self.assertEqual(len(completed_waiter_ids), 2, f"Oczekiwano 2 zakończonych kelnerów, otrzymano {len(completed_waiter_ids)}. Zakończone: {list(completed_waiter_ids)}")
        completed_X_count = sum(1 for pid_str in completed_waiter_ids if pid_str.startswith("Waiter_X"))
        completed_Y_count = sum(1 for pid_str in completed_waiter_ids if pid_str.startswith("Waiter_Y"))
        self.assertEqual(completed_X_count, 1, f"Oczekiwano 1 zakończonego kelnera dla warunku X, otrzymano {completed_X_count}. Zakończone: {list(completed_waiter_ids)}")
        self.assertEqual(completed_Y_count, 1, f"Oczekiwano 1 zakończonego kelnera dla warunku Y, otrzymano {completed_Y_count}. Zakończone: {list(completed_waiter_ids)}")
        self.assertEqual(finished_waiters_count, 2, f"Oczekiwano, że 2 procesy kelnerów zakończą się, a zakończyło się {finished_waiters_count}.")
        print("ok")


if __name__ == '__main__':
    print("🧪 DISTRIBUTED MONITOR - TEST SUITE")
    print("============================================================")
    print("Testy zaprojektowane do precyzyjnej diagnostyki błędów")
    print("============================================================")
    unittest.main(verbosity=0)