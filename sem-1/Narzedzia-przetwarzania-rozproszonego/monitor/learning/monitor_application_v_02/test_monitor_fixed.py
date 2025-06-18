#!/usr/bin/env python3

import unittest
import threading
import time
import logging
import sys
import os
from pathlib import Path

# Dodaj katalog projektu do path
project_root = Path(__file__).parent.parent
sys.path.insert(0, str(project_root))

# Import naszych modułów (będą importowane z artifacts)
try:
    from monitor_server import MonitorServer
    from monitor_client import MonitorClient
except ImportError:
    print("❌ Nie można zaimportować modułów. Upewnij się, że pliki monitor_server.py i monitor_client.py istnieją.")
    sys.exit(1)

class TestBasicConnectivity(unittest.TestCase):
    """Testy podstawowej łączności"""
    
    def setUp(self):
        self.server = MonitorServer(port=5555)
        self.server.start()
        time.sleep(0.1)  # Czas na uruchomienie serwera
    
    def tearDown(self):
        self.server.stop()
        time.sleep(0.1)  # Czas na zamknięcie
    
    def test_01_server_connection(self):
        """Test: Klient może się połączyć z serwerem"""
        client = MonitorClient()
        try:
            status = client.get_status()
            self.assertIsNotNone(status)
            self.assertIsNone(status['mutex_owner'])
        finally:
            client.disconnect()
    
    def test_04_double_enter_error(self):
        """Test: Błąd przy podwójnym enter"""
        client = MonitorClient()
        try:
            client.enter()
            self.assertTrue(client.in_monitor)
            
            # Próba ponownego wejścia powinna rzucić wyjątek
            with self.assertRaises(RuntimeError):
                client.enter()
            
            client.exit()
            
        finally:
            client.disconnect()
    
    def test_05_exit_without_enter_error(self):
        """Test: Błąd przy exit bez enter"""
        client = MonitorClient()
        try:
            # Próba wyjścia bez wejścia powinna rzucić wyjątek
            with self.assertRaises(RuntimeError):
                client.exit()
        finally:
            client.disconnect()

class TestConcurrentAccess(unittest.TestCase):
    """Testy współbieżnego dostępu"""
    
    def setUp(self):
        self.server = MonitorServer(port=5555)
        self.server.start()
        time.sleep(0.1)
        
        # Zmienne do testów
        self.results = []
        self.errors = []
        self.lock = threading.Lock()
    
    def tearDown(self):
        self.server.stop()
        time.sleep(0.1)
    
    def _log_result(self, client_id, action, timestamp):
        """Logowanie wyników z synchronizacją"""
        with self.lock:
            self.results.append({
                'client_id': client_id,
                'action': action,
                'timestamp': timestamp
            })
    
    def _log_error(self, client_id, error):
        """Logowanie błędów z synchronizacją"""
        with self.lock:
            self.errors.append({
                'client_id': client_id,
                'error': str(error)
            })
    
    def _client_worker(self, work_duration=0.5, max_wait=10):
        """Worker function dla klienta"""
        client = MonitorClient()
        try:
            start_time = time.time()
            
            # Próba wejścia do monitora
            client.enter(max_wait_time=max_wait)
            enter_time = time.time()
            self._log_result(client.client_id, 'ENTERED', enter_time)
            
            # Praca w sekcji krytycznej
            time.sleep(work_duration)
            
            # Wyjście z monitora
            client.exit()
            exit_time = time.time()
            self._log_result(client.client_id, 'EXITED', exit_time)
            
        except Exception as e:
            self._log_error(client.client_id if 'client' in locals() else 'unknown', e)
        finally:
            if 'client' in locals():
                client.disconnect()
    
    def test_06_mutual_exclusion(self):
        """Test: Wzajemne wykluczanie między procesami"""
        num_clients = 3
        work_duration = 0.3
        max_wait = 15  # Zwiększony timeout
        
        # Uruchom klientów
        threads = []
        for i in range(num_clients):
            thread = threading.Thread(
                target=self._client_worker,
                args=(work_duration, max_wait),
                name=f"Client-{i}"
            )
            threads.append(thread)
        
        # Start wszystkich wątków
        start_time = time.time()
        for thread in threads:
            thread.start()
        
        # Czekaj na zakończenie wszystkich
        for thread in threads:
            thread.join(timeout=max_wait + 5)
            if thread.is_alive():
                self.fail(f"Thread {thread.name} nie zakończył się w czasie")
        
        total_time = time.time() - start_time
        print(f"\n📊 Test wykonał się w {total_time:.2f}s")
        
        # Sprawdź błędy
        if self.errors:
            error_msg = "Błędy podczas testów:\n"
            for error in self.errors:
                error_msg += f"  - {error['client_id']}: {error['error']}\n"
            self.fail(error_msg)
        
        # Sprawdź czy wszyscy klienci weszli i wyszli
        enters = [r for r in self.results if r['action'] == 'ENTERED']
        exits = [r for r in self.results if r['action'] == 'EXITED']
        
        self.assertEqual(len(enters), num_clients, f"Oczekiwano {num_clients} wejść, otrzymano {len(enters)}")
        self.assertEqual(len(exits), num_clients, f"Oczekiwano {num_clients} wyjść, otrzymano {len(exits)}")
        
        # Sprawdź wzajemne wykluczanie
        self._verify_mutual_exclusion(enters, exits, work_duration)
        
        print(f"✅ Wszyscy {num_clients} klienci pomyślnie przeszli przez monitor")
    
    def _verify_mutual_exclusion(self, enters, exits, work_duration):
        """Weryfikacja wzajemnego wykluczania"""
        # Sortuj wydarzenia po czasie
        enters.sort(key=lambda x: x['timestamp'])
        exits.sort(key=lambda x: x['timestamp'])
        
        print(f"\n🔍 Analiza wzajemnego wykluczenia:")
        print(f"Oczekiwany czas pracy w sekcji: {work_duration}s")
        
        # Sparuj wejścia z wyjściami
        sessions = []
        for enter in enters:
            client_id = enter['client_id']
            matching_exit = next((e for e in exits if e['client_id'] == client_id), None)
            if matching_exit:
                duration = matching_exit['timestamp'] - enter['timestamp']
                sessions.append({
                    'client_id': client_id,
                    'start': enter['timestamp'],
                    'end': matching_exit['timestamp'],
                    'duration': duration
                })
                print(f"  {client_id}: {duration:.3f}s ({enter['timestamp']:.3f} - {matching_exit['timestamp']:.3f})")
        
        # Sprawdź czy żadne dwie sesje się nie nakładają
        for i, session1 in enumerate(sessions):
            for j, session2 in enumerate(sessions[i+1:], i+1):
                overlap = not (session1['end'] <= session2['start'] or session2['end'] <= session1['start'])
                if overlap:
                    self.fail(f"Naruszenie wzajemnego wykluczania: {session1['client_id']} i {session2['client_id']} nakładają się!")
        
        print("✅ Wzajemne wykluczanie zachowane")

class TestConditionVariables(unittest.TestCase):
    """Testy zmiennych warunkowych"""
    
    def setUp(self):
        self.server = MonitorServer(port=5555)
        self.server.start()
        time.sleep(0.1)
    
    def tearDown(self):
        self.server.stop()
        time.sleep(0.1)
    
    def test_07_basic_wait_signal(self):
        """Test: Podstawowe wait/signal"""
        client1 = MonitorClient()
        client2 = MonitorClient()
        
        results = []
        
        def waiter():
            try:
                client1.enter()
                results.append("WAITER_ENTERED")
                client1.wait("test_condition")
                results.append("WAITER_RESUMED")
                client1.exit()
                results.append("WAITER_EXITED")
            except Exception as e:
                results.append(f"WAITER_ERROR: {e}")
            finally:
                client1.disconnect()
        
        def signaler():
            try:
                time.sleep(0.2)  # Poczekaj aż waiter zacznie czekać
                client2.enter()
                results.append("SIGNALER_ENTERED")
                client2.signal("test_condition")
                results.append("SIGNALER_SIGNALED")
                client2.exit()
                results.append("SIGNALER_EXITED")
            except Exception as e:
                results.append(f"SIGNALER_ERROR: {e}")
            finally:
                client2.disconnect()
        
        # Uruchom wątki
        t1 = threading.Thread(target=waiter)
        t2 = threading.Thread(target=signaler)
        
        t1.start()
        t2.start()
        
        t1.join(timeout=10)
        t2.join(timeout=10)
        
        print(f"\n📝 Sekwencja zdarzeń: {results}")
        
        # Sprawdź czy wszystko przebiegło poprawnie
        expected_events = [
            "WAITER_ENTERED", "SIGNALER_ENTERED", "SIGNALER_SIGNALED", 
            "SIGNALER_EXITED", "WAITER_RESUMED", "WAITER_EXITED"
        ]
        
        for event in expected_events:
            self.assertIn(event, results, f"Brakuje zdarzenia: {event}")
    
    def test_08_broadcast(self):
        """Test: Broadcast do wielu oczekujących"""
        num_waiters = 3
        clients = [MonitorClient() for _ in range(num_waiters + 1)]
        results = []
        results_lock = threading.Lock()
        
        def add_result(msg):
            with results_lock:
                results.append(msg)
        
        def waiter(client, waiter_id):
            try:
                client.enter()
                add_result(f"WAITER_{waiter_id}_ENTERED")
                client.wait("broadcast_test")
                add_result(f"WAITER_{waiter_id}_RESUMED")
                client.exit()
                add_result(f"WAITER_{waiter_id}_EXITED")
            except Exception as e:
                add_result(f"WAITER_{waiter_id}_ERROR: {e}")
            finally:
                client.disconnect()
        
        def broadcaster():
            try:
                time.sleep(0.5)  # Poczekaj aż wszyscy zaczną czekać
                clients[-1].enter()
                add_result("BROADCASTER_ENTERED")
                clients[-1].broadcast("broadcast_test")
                add_result("BROADCASTER_BROADCASTED")
                clients[-1].exit()
                add_result("BROADCASTER_EXITED")
            except Exception as e:
                add_result(f"BROADCASTER_ERROR: {e}")
            finally:
                clients[-1].disconnect()
        
        # Uruchom waiters
        waiter_threads = []
        for i in range(num_waiters):
            t = threading.Thread(target=waiter, args=(clients[i], i))
            waiter_threads.append(t)
            t.start()
        
        # Uruchom broadcaster
        broadcaster_thread = threading.Thread(target=broadcaster)
        broadcaster_thread.start()
        
        # Czekaj na zakończenie
        for t in waiter_threads:
            t.join(timeout=15)
        broadcaster_thread.join(timeout=15)
        
        print(f"\n📝 Wyniki broadcast: {len(results)} zdarzeń")
        for result in results:
            print(f"  {result}")
        
        # Sprawdź czy wszyscy zostali obudzeni
        resumed_count = len([r for r in results if "_RESUMED" in r])
        self.assertEqual(resumed_count, num_waiters, f"Oczekiwano {num_waiters} obudzonych, otrzymano {resumed_count}")

if __name__ == "__main__":
    # Konfiguracja logowania
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    )
    
    print("🧪 DISTRIBUTED MONITOR - TEST SUITE")
    print("=" * 60)
    print("Testy zmodernizowane z lepszą diagnostyką błędów")
    print("=" * 60)
    
    # Uruchom testy
    unittest.main(verbosity=2, exit=False)
    
    print("\n" + "=" * 60)
    print("🎯 KONIEC TESTÓW")
    print("=" * 60)()
    
    def test_02_server_status(self):
        """Test: Pobieranie stanu serwera"""
        client = MonitorClient()
        try:
            status = client.get_status()
            expected_keys = ['mutex_owner', 'mutex_queue', 'clients_in_monitor', 'condition_queues']
            for key in expected_keys:
                self.assertIn(key, status)
        finally:
            client.disconnect()

class TestMutexOperations(unittest.TestCase):
    """Testy operacji mutex"""
    
    def setUp(self):
        self.server = MonitorServer(port=5555)
        self.server.start()
        time.sleep(0.1)
    
    def tearDown(self):
        self.server.stop()
        time.sleep(0.1)
    
    def test_03_basic_enter_exit(self):
        """Test: Podstawowe wejście/wyjście z monitora"""
        client = MonitorClient()
        try:
            # Wejście
            client.enter()
            self.assertTrue(client.in_monitor)
            
            # Sprawdź status
            status = client.get_status()
            self.assertEqual(status['mutex_owner'], client.client_id)
            self.assertIn(client.client_id, status['clients_in_monitor'])
            
            # Wyjście
            client.exit()
            self.assertFalse(client.in_monitor)
            
            # Sprawdź status po wyjściu
            status = client.get_status()
            self.assertIsNone(status['mutex_owner'])
            self.assertEqual(len(status['clients_in_monitor']), 0)
            
        finally:
            client.disconnect