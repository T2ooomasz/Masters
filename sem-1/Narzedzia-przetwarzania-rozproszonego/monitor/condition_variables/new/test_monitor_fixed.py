#!/usr/bin/env python3
"""
Poprawione testy dla Distributed Monitor
Rozwiązuje problemy z serializacją funkcji lokalnych na Windows
"""

import unittest
import multiprocessing
import time
import threading
import subprocess
import sys
import os
from monitor_client import MonitorClient, synchronized, MonitorError
from monitor_server import MonitorServer

# Ustawienia dla Windows
if sys.platform.startswith('win'):
    multiprocessing.set_start_method('spawn', force=True)

print("🧪 DISTRIBUTED MONITOR - TEST SUITE")
print("=" * 60)
print("Testy zaprojektowane do precyzyjnej diagnostyki błędów")
print("=" * 60)

class TestBasicConnectivity(unittest.TestCase):
    """Testy podstawowej łączności"""
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.server_process = None
        self.start_server()
        time.sleep(0.3)  # Czas na uruchomienie serwera
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        if self.server_process:
            self.server_process.terminate()
            self.server_process.join(timeout=2)
            if self.server_process.is_alive():
                self.server_process.kill()
    
    def start_server(self):
        """Uruchomienie serwera w osobnym procesie"""
        def run_server():
            server = MonitorServer(port=5555)
            server.start()
        
        self.server_process = multiprocessing.Process(target=run_server)
        self.server_process.start()
    
    def test_01_server_connection(self):
        """Test: Klient może się połączyć z serwerem"""
        client = MonitorClient()
        try:
            client.connect()
            status = client.get_status()
            self.assertIsInstance(status, dict)
        finally:
            client.disconnect()
    
    def test_02_server_status(self):
        """Test: Pobieranie stanu serwera"""
        client = MonitorClient()
        try:
            client.connect()
            status = client.get_status()
            
            self.assertIn('mutex_owner', status)
            self.assertIn('active_clients', status)
            self.assertIsNone(status['mutex_owner'])
            self.assertEqual(len(status['active_clients']), 0)
        finally:
            client.disconnect()

class TestMutexOperations(unittest.TestCase):
    """Testy operacji mutex"""
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.server_process = None
        self.start_server()
        time.sleep(0.3)
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        if self.server_process:
            self.server_process.terminate()
            self.server_process.join(timeout=2)
            if self.server_process.is_alive():
                self.server_process.kill()
    
    def start_server(self):
        """Uruchomienie serwera w osobnym procesie"""
        def run_server():
            server = MonitorServer(port=5555)
            server.start()
        
        self.server_process = multiprocessing.Process(target=run_server)
        self.server_process.start()
    
    def test_03_basic_enter_exit(self):
        """Test: Podstawowe wejście/wyjście z monitora"""
        client = MonitorClient()
        try:
            client.enter()
            self.assertTrue(client.in_monitor)
            
            client.exit()
            self.assertFalse(client.in_monitor)
        finally:
            client.disconnect()
    
    def test_04_double_enter_error(self):
        """Test: Błąd przy podwójnym enter"""
        client = MonitorClient()
        try:
            client.enter()
            
            # Próba ponownego wejścia powinna rzucić wyjątek
            with self.assertRaises(MonitorError):
                client.enter()
            
            client.exit()
        finally:
            client.disconnect()
    
    def test_05_exit_without_enter_error(self):
        """Test: Błąd przy exit bez enter"""
        client = MonitorClient()
        try:
            # Próba wyjścia bez wejścia powinna rzucić wyjątek
            with self.assertRaises(MonitorError):
                client.exit()
        finally:
            client.disconnect()

# Funkcje worker dla testów multiprocessing - muszą być na poziomie modułu
def mutual_exclusion_worker(results, worker_id, duration=0.5):
    """Worker dla testu wzajemnego wykluczania"""
    print(f"🔧 TEST: Mutual exclusion")
    
    client = MonitorClient()
    try:
        client.enter()
        
        # Symulacja pracy w sekcji krytycznej
        start_time = time.time()
        results[worker_id] = {
            'start': start_time,
            'end': None,
            'in_critical_section': True
        }
        
        time.sleep(duration)
        
        end_time = time.time()
        results[worker_id]['end'] = end_time
        results[worker_id]['in_critical_section'] = False
        
        client.exit()
        
    except Exception as e:
        results[worker_id] = {'error': str(e)}
    finally:
        client.disconnect()

def wait_releases_mutex_waiter(barrier, condition_name="test_condition"):
    """Worker który oczekuje na warunek"""
    print(f"🔧 TEST: Wait zwalnia mutex")
    
    client = MonitorClient()
    try:
        client.enter()
        
        # Sygnalizuj że jestem gotowy
        barrier.wait()
        
        # Oczekuj na warunek - powinno zwolnić mutex
        client.wait(condition_name)
        
        client.exit()
        
    except Exception as e:
        print(f"Błąd w waiter: {e}")
    finally:
        client.disconnect()

def wait_releases_mutex_checker(barrier, results, condition_name="test_condition"):
    """Worker który sprawdza czy mutex został zwolniony"""
    client = MonitorClient()
    try:
        # Poczekaj aż waiter będzie gotowy
        barrier.wait()
        
        # Krótka przerwa żeby waiter zaczął wait()
        time.sleep(0.2)
        
        # Spróbuj wejść do monitora - powinno się udać jeśli wait() zwolnił mutex
        client.enter()
        results['checker_entered'] = True
        
        # Wyślij sygnał żeby obudzić waitera
        client.signal(condition_name)
        
        client.exit()
        
    except Exception as e:
        results['error'] = str(e)
    finally:
        client.disconnect()

def signal_wakes_waiter(barrier, results, worker_id, condition_name="test_condition"):
    """Worker który oczekuje na sygnał"""
    print(f"🔧 TEST: Signal budzi proces")
    
    client = MonitorClient()
    try:
        client.enter()
        
        # Sygnalizuj gotowość
        barrier.wait()
        
        start_time = time.time()
        results[worker_id] = {'start': start_time, 'awakened': False}
        
        # Oczekuj na warunek
        client.wait(condition_name)
        
        # Zostaliśmy obudzeni
        end_time = time.time()
        results[worker_id]['end'] = end_time
        results[worker_id]['awakened'] = True
        
        client.exit()
        
    except Exception as e:
        results[worker_id] = {'error': str(e)}
    finally:
        client.disconnect()

def signal_wakes_signaler(barrier, condition_name="test_condition"):
    """Worker który wysyła sygnał"""
    client = MonitorClient()
    try:
        # Poczekaj aż waiters będą gotowi
        barrier.wait()
        
        # Krótka przerwa żeby waiters zaczęli wait()
        time.sleep(0.3)
        
        client.enter()
        client.signal(condition_name)
        client.exit()
        
    except Exception as e:
        print(f"Błąd w signaler: {e}")
    finally:
        client.disconnect()

class TestConcurrentAccess(unittest.TestCase):
    """Testy współbieżnego dostępu"""
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.server_process = None
        self.start_server()
        time.sleep(0.3)
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        if self.server_process:
            self.server_process.terminate()
            self.server_process.join(timeout=2)
            if self.server_process.is_alive():
                self.server_process.kill()
    
    def start_server(self):
        """Uruchomienie serwera w osobnym procesie"""
        def run_server():
            server = MonitorServer(port=5555)
            server.start()
        
        self.server_process = multiprocessing.Process(target=run_server)
        self.server_process.start()
    
    def test_06_mutual_exclusion(self):
        """Test: Wzajemne wykluczanie między procesami"""
        manager = multiprocessing.Manager()
        results = manager.dict()
        
        processes = []
        for i in range(3):
            p = multiprocessing.Process(
                target=mutual_exclusion_worker, 
                args=(results, i, 0.3)
            )
            processes.append(p)
        
        # Uruchom wszystkie procesy
        for p in processes:
            p.start()
        
        # Poczekaj na zakończenie
        for p in processes:
            p.join(timeout=10)
            if p.is_alive():
                p.terminate()
        
        # Sprawdź wyniki
        self.assertEqual(len(results), 3)
        
        # Sprawdź czy nie było błędów
        for worker_id, result in results.items():
            self.assertNotIn('error', result, f"Worker {worker_id} miał błąd: {result.get('error')}")
        
        # Sprawdź czy sekcje krytyczne nie nakładały się
        times = [(r['start'], r['end']) for r in results.values() if 'start' in r and 'end' in r]
        
        for i, (start1, end1) in enumerate(times):
            for j, (start2, end2) in enumerate(times):
                if i != j:
                    # Sprawdź czy przedziały się nie nakładają
                    overlaps = not (end1 <= start2 or end2 <= start1)
                    self.assertFalse(overlaps, f"Sekcje krytyczne {i} i {j} się nakładają")

class TestConditionVariables(unittest.TestCase):
    """Testy zmiennych warunków"""
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.server_process = None
        self.start_server()
        time.sleep(0.3)
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        if self.server_process:
            self.server_process.terminate()
            self.server_process.join(timeout=2)
            if self.server_process.is_alive():
                self.server_process.kill()
    
    def start_server(self):
        """Uruchomienie serwera w osobnym procesie"""
        def run_server():
            server = MonitorServer(port=5555)
            server.start()
        
        self.server_process = multiprocessing.Process(target=run_server)
        self.server_process.start()
    
    def test_07_wait_without_mutex_error(self):
        """Test: Błąd wait bez mutex"""
        client = MonitorClient()
        try:
            with self.assertRaises(MonitorError):
                client.wait("test_condition")
        finally:
            client.disconnect()
    
    def test_08_signal_without_mutex_error(self):
        """Test: Błąd signal bez mutex"""
        client = MonitorClient()
        try:
            with self.assertRaises(MonitorError):
                client.signal("test_condition")
        finally:
            client.disconnect()
    
    def test_09_wait_releases_mutex(self):
        """Test: Wait zwalnia mutex"""
        barrier = multiprocessing.Barrier(2)
        manager = multiprocessing.Manager()
        results = manager.dict()
        
        # Proces który będzie oczekiwał
        p1 = multiprocessing.Process(
            target=wait_releases_mutex_waiter,
            args=(barrier,)
        )
        
        # Proces który sprawdzi czy mutex został zwolniony
        p2 = multiprocessing.Process(
            target=wait_releases_mutex_checker,
            args=(barrier, results)
        )
        
        p1.start()
        p2.start()
        
        p1.join(timeout=10)
        p2.join(timeout=10)
        
        if p1.is_alive():
            p1.terminate()
        if p2.is_alive():
            p2.terminate()
        
        # Sprawdź czy checker zdołał wejść do monitora
        self.assertIn('checker_entered', results)
        self.assertTrue(results['checker_entered'])
        self.assertNotIn('error', results)
    
    def test_10_signal_wakes_waiting_process(self):
        """Test: Signal budzi oczekujący proces"""
        barrier = multiprocessing.Barrier(3)  # 2 waiters + 1 signaler
        manager = multiprocessing.Manager()
        results = manager.dict()
        
        # Dwa procesy oczekujące
        processes = []
        for i in range(2):
            p = multiprocessing.Process(
                target=signal_wakes_waiter,
                args=(barrier, results, i)
            )
            processes.append(p)
        
        # Proces wysyłający sygnał
        signaler = multiprocessing.Process(
            target=signal_wakes_signaler,
            args=(barrier,)
        )
        processes.append(signaler)
        
        # Uruchom wszystkie
        for p in processes:
            p.start()
        
        # Poczekaj na zakończenie
        for p in processes:
            p.join(timeout=15)
            if p.is_alive():
                p.terminate()
        
        # Sprawdź wyniki - przynajmniej jeden proces powinien zostać obudzony
        awakened_count = sum(1 for r in results.values() if isinstance(r, dict) and r.get('awakened'))
        self.assertGreaterEqual(awakened_count, 1, "Żaden proces nie został obudzony")

class TestContextManagers(unittest.TestCase):
    """Testy context managerów"""
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.server_process = None
        self.start_server()
        time.sleep(0.3)
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        if self.server_process:
            self.server_process.terminate()
            self.server_process.join(timeout=2)
            if self.server_process.is_alive():
                self.server_process.kill()
    
    def start_server(self):
        """Uruchomienie serwera w osobnym procesie"""
        def run_server():
            server = MonitorServer(port=5555)
            server.start()
        
        self.server_process = multiprocessing.Process(target=run_server)
        self.server_process.start()
    
    def test_11_synchronized_context_manager(self):
        """Test: synchronized() context manager"""
        client = MonitorClient()
        
        try:
            with synchronized(client) as monitor:
                self.assertTrue(monitor.in_monitor)
            
            # Po wyjściu z context managera, powinniśmy być poza monitorem
            self.assertFalse(client.in_monitor)
        finally:
            client.disconnect()
    
    def test_12_with_statement(self):
        """Test: with statement support"""
        with MonitorClient() as client:
            client.enter()
            self.assertTrue(client.in_monitor)
            client.exit()

def run_tests():
    """Uruchomienie wszystkich testów"""
    # Konfiguracja unittest
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Dodaj wszystkie klasy testów
    test_classes = [
        TestBasicConnectivity,
        TestMutexOperations,
        TestConcurrentAccess,
        TestConditionVariables,
        TestContextManagers
    ]
    
    for test_class in test_classes:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    
    # Uruchom testy
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Podsumowanie
    print("\n" + "=" * 60)
    print("PODSUMOWANIE TESTÓW")
    print("=" * 60)
    
    if result.failures:
        print(f"❌ NIEPOWODZENIA: {len(result.failures)}")
        for test, traceback in result.failures:
            print(f"   - {test}: {traceback.split('AssertionError: ')[-1].split('\n')[0]}")
    else:
        print("❌ NIEPOWODZENIA: 0")
    
    if result.errors:
        print(f"💥 BŁĘDY: {len(result.errors)}")
        print("\nSzczegóły błędów powyżej zawierają dokładne lokalizacje problemów.")
    else:
        print("💥 BŁĘDY: 0")
    
    print(f"📊 Wykonano: {result.testsRun} testów")
    
    return result.wasSuccessful()

if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)