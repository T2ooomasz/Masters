#!/usr/bin/env python3
"""
Test Suite dla Distributed Monitor - Etap 3
Kompleksowe testy funkcjonalności monitora rozproszonego z condition variables.

Testy zaprojektowane tak, aby w przypadku niepowodzenia dawały precyzyjne
informacje o lokalizacji problemu i jego przyczynie.
"""

import unittest
import threading
import time
import subprocess
import sys
import signal
import os
from unittest.mock import patch
import queue
import multiprocessing as mp

# Import testowanych modułów
try:
    from monitor_client_step3 import DistributedMonitor, DistributedMonitorError
    from monitor_server_step3 import MonitorServer
except ImportError as e:
    print(f"BŁĄD IMPORTU: {e}")
    print("Upewnij się, że pliki monitor_client_step3.py i monitor_server_step3.py są w tym samym katalogu")
    sys.exit(1)

class TestDistributedMonitor(unittest.TestCase):
    """Test suite dla distributed monitor"""
    
    @classmethod
    def setUpClass(cls):
        """Setup serwera monitora dla wszystkich testów"""
        print("\n" + "="*60)
        print("URUCHAMIANIE SERWERA MONITORA...")
        print("="*60)
        
        # Uruchom serwer w osobnym procesie
        cls.server_process = mp.Process(target=cls._run_server, daemon=True)
        cls.server_process.start()
        
        # Daj serwerowi czas na uruchomienie
        time.sleep(0.5)
        
        # Sprawdź czy serwer się uruchomił
        if not cls.server_process.is_alive():
            raise Exception("KRYTYCZNY BŁĄD: Nie udało się uruchomić serwera monitora")
        
        print("✅ Serwer monitora uruchomiony poprawnie")
    
    @classmethod
    def _run_server(cls):
        """Uruchomienie serwera monitora"""
        server = MonitorServer(port=5555)
        try:
            server.start()
        except KeyboardInterrupt:
            server.stop()
    
    @classmethod
    def tearDownClass(cls):
        """Zatrzymanie serwera monitora"""
        print("\n" + "="*60)
        print("ZATRZYMYWANIE SERWERA MONITORA...")
        print("="*60)
        
        if hasattr(cls, 'server_process') and cls.server_process.is_alive():
            cls.server_process.terminate()
            cls.server_process.join(timeout=2)
            if cls.server_process.is_alive():
                cls.server_process.kill()
        print("✅ Serwer monitora zatrzymany")
    
    def setUp(self):
        """Setup przed każdym testem"""
        self.monitor = DistributedMonitor("tcp://localhost:5555", timeout=2000)
        # Krótkie opóźnienie między testami
        time.sleep(0.1)
    
    def tearDown(self):
        """Cleanup po każdym teście"""
        try:
            if hasattr(self.monitor, 'has_mutex') and self.monitor.has_mutex:
                self.monitor.exit()
            self.monitor.disconnect()
        except:
            pass

class TestBasicConnectivity(TestDistributedMonitor):
    """Testy podstawowej łączności"""
    
    def test_01_server_connection(self):
        """Test: Klient może się połączyć z serwerem"""
        print("\n🔧 TEST: Połączenie z serwerem")
        
        try:
            self.monitor.connect()
            self.assertTrue(self.monitor.is_connected)
            print("✅ Połączenie nawiązane poprawnie")
        except Exception as e:
            self.fail(f"❌ BŁĄD POŁĄCZENIA: {e}\n"
                     f"   Sprawdź czy serwer działa na porcie 5555")
    
    def test_02_server_status(self):
        """Test: Pobieranie stanu serwera"""
        print("\n🔧 TEST: Status serwera")
        
        try:
            status = self.monitor.get_server_status()
            
            # Sprawdź strukturę odpowiedzi
            required_keys = ['status', 'mutex_owner', 'mutex_queue', 'condition_queues']
            missing_keys = [key for key in required_keys if key not in status]
            
            if missing_keys:
                self.fail(f"❌ BŁĄD STRUKTURY ODPOWIEDZI: Brakuje kluczy {missing_keys}\n"
                         f"   Otrzymano: {status}\n"
                         f"   LOKALIZACJA: MonitorServer._handle_get_status()")
            
            self.assertEqual(status['status'], 'OK')
            print(f"✅ Status serwera OK: {status}")
            
        except Exception as e:
            self.fail(f"❌ BŁĄD POBIERANIA STATUSU: {e}\n"
                     f"   LOKALIZACJA: Prawdopodobnie MonitorServer._handle_get_status()")

class TestMutexOperations(TestDistributedMonitor):
    """Testy operacji mutex (enter/exit)"""
    
    def test_03_basic_enter_exit(self):
        """Test: Podstawowe wejście/wyjście z monitora"""
        print("\n🔧 TEST: Podstawowe enter/exit")
        
        try:
            # Sprawdź stan początkowy
            self.assertFalse(self.monitor.has_mutex)
            
            # Wejście do monitora
            result = self.monitor.enter()
            self.assertTrue(result)
            self.assertTrue(self.monitor.has_mutex)
            print("✅ Enter: mutex przyznany")
            
            # Sprawdź status serwera
            status = self.monitor.get_server_status()
            if status['mutex_owner'] != self.monitor.client_id:
                self.fail(f"❌ BŁĄD STANU SERWERA: mutex_owner={status['mutex_owner']}, "
                         f"expected={self.monitor.client_id}\n"
                         f"   LOKALIZACJA: MonitorServer._handle_enter_monitor()")
            
            # Wyjście z monitora
            result = self.monitor.exit()
            self.assertTrue(result)
            self.assertFalse(self.monitor.has_mutex)
            print("✅ Exit: mutex zwolniony")
            
            # Sprawdź status serwera po wyjściu
            status = self.monitor.get_server_status()
            if status['mutex_owner'] is not None:
                self.fail(f"❌ BŁĄD STANU SERWERA: mutex nie został zwolniony, "
                         f"mutex_owner={status['mutex_owner']}\n"
                         f"   LOKALIZACJA: MonitorServer._handle_exit_monitor()")
            
        except Exception as e:
            self.fail(f"❌ BŁĄD ENTER/EXIT: {e}\n"
                     f"   LOKALIZACJA: DistributedMonitor.enter() lub .exit()")
    
    def test_04_double_enter_error(self):
        """Test: Błąd przy podwójnym enter"""
        print("\n🔧 TEST: Błąd podwójnego enter")
        
        try:
            self.monitor.enter()
            
            # Próba ponownego enter powinna zgłosić błąd
            with self.assertRaises(DistributedMonitorError) as cm:
                self.monitor.enter()
            
            error_msg = str(cm.exception)
            if "już ma mutex" not in error_msg:
                self.fail(f"❌ BŁĄD KOMUNIKATU: Oczekiwano 'już ma mutex', otrzymano '{error_msg}'\n"
                         f"   LOKALIZACJA: DistributedMonitor.enter() - walidacja stanu")
            
            print("✅ Podwójne enter poprawnie odrzucone")
            
        except Exception as e:
            if isinstance(e, AssertionError):
                raise
            self.fail(f"❌ NIEOCZEKIWANY BŁĄD: {e}")
    
    def test_05_exit_without_enter_error(self):
        """Test: Błąd przy exit bez enter"""
        print("\n🔧 TEST: Błąd exit bez enter")
        
        try:
            with self.assertRaises(DistributedMonitorError) as cm:
                self.monitor.exit()
            
            error_msg = str(cm.exception)
            if "nie ma mutex" not in error_msg:
                self.fail(f"❌ BŁĄD KOMUNIKATU: Oczekiwano 'nie ma mutex', otrzymano '{error_msg}'\n"
                         f"   LOKALIZACJA: DistributedMonitor.exit() - walidacja stanu")
            
            print("✅ Exit bez enter poprawnie odrzucony")
            
        except Exception as e:
            if isinstance(e, AssertionError):
                raise
            self.fail(f"❌ NIEOCZEKIWANY BŁĄD: {e}")

class TestConcurrentAccess(TestDistributedMonitor):
    """Testy konkurencyjnego dostępu"""
    
    def test_06_mutual_exclusion(self):
        """Test: Wzajemne wykluczanie między procesami"""
        print("\n🔧 TEST: Mutual exclusion")
        
        # Używamy queue do komunikacji z subprocesami
        result_queue = mp.Queue()
        
        def worker(worker_id, duration):
            """Worker process - próbuje wejść do monitora"""
            try:
                monitor = DistributedMonitor("tcp://localhost:5555", timeout=3000)
                
                start_time = time.time()
                monitor.enter()
                enter_time = time.time()
                
                # Symulacja pracy w sekcji krytycznej
                time.sleep(duration)
                
                monitor.exit()
                exit_time = time.time()
                
                result_queue.put({
                    'worker_id': worker_id,
                    'success': True,
                    'start_time': start_time,
                    'enter_time': enter_time,
                    'exit_time': exit_time,
                    'duration': duration
                })
                
            except Exception as e:
                result_queue.put({
                    'worker_id': worker_id,
                    'success': False,
                    'error': str(e)
                })
        
        # Uruchom 3 procesy równocześnie
        processes = []
        durations = [0.3, 0.2, 0.1]  # Różne czasy pracy
        
        for i, duration in enumerate(durations):
            p = mp.Process(target=worker, args=(i, duration))
            processes.append(p)
            p.start()
        
        # Czekaj na zakończenie
        for p in processes:
            p.join(timeout=10)
            if p.is_alive():
                p.terminate()
                self.fail(f"❌ TIMEOUT: Proces {p.pid} nie zakończył się w czasie\n"
                         f"   LOKALIZACJA: Prawdopodobnie deadlock w monitorze")
        
        # Zbierz wyniki
        results = []
        while not result_queue.empty():
            results.append(result_queue.get())
        
        # Sprawdź czy wszystkie procesy zakończone pomyślnie
        failed = [r for r in results if not r['success']]
        if failed:
            errors = [f"Worker {r['worker_id']}: {r['error']}" for r in failed]
            self.fail(f"❌ BŁĘDY W PROCESACH:\n" + "\n".join(errors))
        
        # Sprawdź mutual exclusion - czasy wejścia/wyjścia nie powinny się nakładać
        results.sort(key=lambda x: x['enter_time'])
        
        for i in range(len(results) - 1):
            current = results[i]
            next_worker = results[i + 1]
            
            if current['exit_time'] > next_worker['enter_time']:
                self.fail(f"❌ NARUSZENIE MUTUAL EXCLUSION:\n"
                         f"   Worker {current['worker_id']}: exit={current['exit_time']:.3f}\n"
                         f"   Worker {next_worker['worker_id']}: enter={next_worker['enter_time']:.3f}\n"
                         f"   LOKALIZACJA: MonitorServer - zarządzanie mutex")
        
        print(f"✅ Mutual exclusion zachowane dla {len(results)} procesów")

class TestConditionVariables(TestDistributedMonitor):
    """Testy condition variables"""
    
    def test_07_wait_without_mutex_error(self):
        """Test: Błąd wait bez mutex"""
        print("\n🔧 TEST: Wait bez mutex")
        
        try:
            with self.assertRaises(DistributedMonitorError) as cm:
                self.monitor.wait("test_condition")
            
            error_msg = str(cm.exception)
            if "Musisz mieć mutex" not in error_msg:
                self.fail(f"❌ BŁĄD KOMUNIKATU: Oczekiwano 'Musisz mieć mutex', otrzymano '{error_msg}'\n"
                         f"   LOKALIZACJA: DistributedMonitor.wait() - walidacja stanu")
            
            print("✅ Wait bez mutex poprawnie odrzucony")
            
        except Exception as e:
            if isinstance(e, AssertionError):
                raise
            self.fail(f"❌ NIEOCZEKIWANY BŁĄD: {e}")
    
    def test_08_signal_without_mutex_error(self):
        """Test: Błąd signal bez mutex"""
        print("\n🔧 TEST: Signal bez mutex")
        
        try:
            with self.assertRaises(DistributedMonitorError) as cm:
                self.monitor.signal("test_condition")
            
            error_msg = str(cm.exception)
            if "Musisz mieć mutex" not in error_msg:
                self.fail(f"❌ BŁĄD KOMUNIKATU: Oczekiwano 'Musisz mieć mutex', otrzymano '{error_msg}'\n"
                         f"   LOKALIZACJA: DistributedMonitor.signal() - walidacja stanu")
            
            print("✅ Signal bez mutex poprawnie odrzucony")
            
        except Exception as e:
            if isinstance(e, AssertionError):
                raise
            self.fail(f"❌ NIEOCZEKIWANY BŁĄD: {e}")
    
    def test_09_wait_releases_mutex(self):
        """Test: Wait zwalnia mutex"""
        print("\n🔧 TEST: Wait zwalnia mutex")
        
        result_queue = mp.Queue()
        
        def waiter():
            """Proces oczekujący na warunek"""
            try:
                monitor = DistributedMonitor("tcp://localhost:5555", timeout=5000)
                monitor.enter()
                
                result_queue.put(('waiter_entered', time.time()))
                
                # Wait powinien zwolnić mutex
                monitor.wait("test_condition")
                
                result_queue.put(('waiter_woken', time.time()))
                monitor.exit()
                result_queue.put(('waiter_success', True))
                
            except Exception as e:
                result_queue.put(('waiter_error', str(e)))
        
        def signaler():
            """Proces sygnalizujący"""
            try:
                time.sleep(0.2)  # Daj waiter czas na wait()
                
                monitor = DistributedMonitor("tcp://localhost:5555", timeout=5000)
                monitor.enter()  # To powinno się udać jeśli wait() zwolnił mutex
                
                result_queue.put(('signaler_entered', time.time()))
                
                monitor.signal("test_condition")
                monitor.exit()
                result_queue.put(('signaler_success', True))
                
            except Exception as e:
                result_queue.put(('signaler_error', str(e)))
        
        # Uruchom procesy
        p1 = mp.Process(target=waiter)
        p2 = mp.Process(target=signaler)
        
        p1.start()
        p2.start()
        
        # Czekaj na zakończenie
        p1.join(timeout=10)
        p2.join(timeout=10)
        
        if p1.is_alive() or p2.is_alive():
            p1.terminate()
            p2.terminate()
            self.fail("❌ TIMEOUT: Procesy nie zakończyły się w czasie\n"
                     "   LOKALIZACJA: Prawdopodobnie wait() nie zwalnia mutex")
        
        # Sprawdź wyniki
        events = {}
        while not result_queue.empty():
            event, data = result_queue.get()
            events[event] = data
        
        # Sprawdź błędy
        if 'waiter_error' in events:
            self.fail(f"❌ BŁĄD WAITER: {events['waiter_error']}\n"
                     f"   LOKALIZACJA: DistributedMonitor.wait()")
        
        if 'signaler_error' in events:
            self.fail(f"❌ BŁĄD SIGNALER: {events['signaler_error']}\n"
                     f"   LOKALIZACJA: DistributedMonitor.enter() po wait() lub signal()")
        
        # Sprawdź czy signaler wszedł do monitora (dowód że wait zwolnił mutex)
        if 'signaler_entered' not in events:
            self.fail("❌ SIGNALER NIE WSZEDŁ: wait() prawdopodobnie nie zwolnił mutex\n"
                     "   LOKALIZACJA: MonitorServer._handle_wait_condition()")
        
        print("✅ Wait poprawnie zwalnia mutex")
    
    def test_10_signal_wakes_waiting_process(self):
        """Test: Signal budzi oczekujący proces"""
        print("\n🔧 TEST: Signal budzi proces")
        
        result_queue = mp.Queue()
        
        def waiter():
            """Proces oczekujący"""
            try:
                monitor = DistributedMonitor("tcp://localhost:5555", timeout=10000)
                monitor.enter()
                
                result_queue.put(('wait_start', time.time()))
                monitor.wait("wake_me_up")
                result_queue.put(('wait_end', time.time()))
                
                monitor.exit()
                result_queue.put(('waiter_done', True))
                
            except Exception as e:
                result_queue.put(('waiter_error', str(e)))
        
        def signaler():
            """Proces sygnalizujący"""
            try:
                time.sleep(0.5)  # Daj waiter czas na wait()
                
                monitor = DistributedMonitor("tcp://localhost:5555", timeout=5000)
                monitor.enter()
                
                result_queue.put(('signal_sent', time.time()))
                monitor.signal("wake_me_up")
                
                monitor.exit()
                result_queue.put(('signaler_done', True))
                
            except Exception as e:
                result_queue.put(('signaler_error', str(e)))
        
        # Uruchom procesy
        p1 = mp.Process(target=waiter)
        p2 = mp.Process(target=signaler)
        
        p1.start()
        p2.start()
        
        # Czekaj na zakończenie
        p1.join(timeout=15)
        p2.join(timeout=5)
        
        if p1.is_alive() or p2.is_alive():
            p1.terminate()
            p2.terminate()
            self.fail("❌ TIMEOUT: Signal nie obudził procesu\n"
                     "   LOKALIZACJA: MonitorServer._handle_signal_condition()")
        
        # Sprawdź wyniki
        events = {}
        while not result_queue.empty():
            event, data = result_queue.get()
            events[event] = data
        
        # Sprawdź błędy
        if 'waiter_error' in events:
            self.fail(f"❌ BŁĄD WAITER: {events['waiter_error']}")
        
        if 'signaler_error' in events:
            self.fail(f"❌ BŁĄD SIGNALER: {events['signaler_error']}")
        
        # Sprawdź czy waiter został obudzony po signal
        if 'wait_end' not in events or 'signal_sent' not in events:
            self.fail("❌ BRAK WYDARZEŃ: Nie można zweryfikować czy signal obudził proces")
        
        if events['wait_end'] < events['signal_sent']:
            self.fail("❌ BŁĄD SYNCHRONIZACJI: Waiter obudził się przed signal\n"
                     "   LOKALIZACJA: MonitorServer - zarządzanie kolejkami warunków")
        
        print("✅ Signal poprawnie budzi oczekujący proces")

class TestContextManagers(TestDistributedMonitor):
    """Testy context managerów"""
    
    def test_11_synchronized_context_manager(self):
        """Test: synchronized() context manager"""
        print("\n🔧 TEST: synchronized() context manager")
        
        try:
            with self.monitor.synchronized():
                self.assertTrue(self.monitor.has_mutex)
                print("✅ W bloku synchronized: mutex przyznany")
            
            self.assertFalse(self.monitor.has_mutex)
            print("✅ Po bloku synchronized: mutex zwolniony")
            
        except Exception as e:
            self.fail(f"❌ BŁĄD CONTEXT MANAGER: {e}\n"
                     f"   LOKALIZACJA: DistributedMonitor.synchronized()")
    
    def test_12_with_statement(self):
        """Test: with statement support"""
        print("\n🔧 TEST: with statement")
        
        try:
            with self.monitor:
                self.assertTrue(self.monitor.has_mutex)
                print("✅ W bloku with: mutex przyznany")
            
            self.assertFalse(self.monitor.has_mutex)
            print("✅ Po bloku with: mutex zwolniony")
            
        except Exception as e:
            self.fail(f"❌ BŁĄD WITH STATEMENT: {e}\n"
                     f"   LOKALIZACJA: DistributedMonitor.__enter__/__exit__")

def run_tests():
    """Uruchomienie wszystkich testów z szczegółowym raportowaniem"""
    
    print("🧪 DISTRIBUTED MONITOR - TEST SUITE")
    print("="*60)
    print("Testy zaprojektowane do precyzyjnej diagnostyki błędów")
    print("="*60)
    
    # Test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Dodaj testy w określonej kolejności
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
    
    # Custom test runner z lepszym formatowaniem
    runner = unittest.TextTestRunner(
        verbosity=2,
        stream=sys.stdout,
        buffer=True
    )
    
    result = runner.run(suite)
    
    # Podsumowanie
    print("\n" + "="*60)
    print("PODSUMOWANIE TESTÓW")
    print("="*60)
    
    if result.wasSuccessful():
        print("🎉 WSZYSTKIE TESTY PRZESZŁY POMYŚLNIE!")
        print("✅ Distributed Monitor działa poprawnie")
    else:
        print(f"❌ NIEPOWODZENIA: {len(result.failures)}")
        print(f"💥 BŁĘDY: {len(result.errors)}")
        print("\nSzczegóły błędów powyżej zawierają dokładne lokalizacje problemów.")
    
    print(f"📊 Wykonano: {result.testsRun} testów")
    print("="*60)
    
    return result.wasSuccessful()

if __name__ == "__main__":
    try:
        success = run_tests()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n\n⚠️ Przerwano przez użytkownika")
        sys.exit(1)
    except Exception as e:
        print(f"\n💥 KRYTYCZNY BŁĄD TESTÓW: {e}")
        sys.exit(1)