#!/usr/bin/env python3
"""
Test Runner - Etap 3: Monitor z Condition Variables
Automatyczny test funkcjonalności distributed monitor z condition variables.

Scenariusze testowe:
1. Basic Test - podstawowa funkcjonalność wait/signal
2. Producer-Consumer - bounded buffer z wieloma wątkami
3. Broadcast Test - testowanie broadcast vs signal
4. Stress Test - duże obciążenie systemu
"""

import subprocess
import time
import threading
import sys
import os
from pathlib import Path

class TestRunner:
    """Klasa do zarządzania testami distributed monitor"""
    
    def __init__(self):
        self.server_process = None
        self.server_port = 5555
        
    def start_server(self):
        """Uruchomienie serwera monitora"""
        print("🚀 Uruchamianie Monitor Server...")
        try:
            self.server_process = subprocess.Popen([
                sys.executable, 'monitor_server_step3.py', 
                '--port', str(self.server_port)
            ], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            
            # Krótkie oczekiwanie na uruchomienie serwera
            time.sleep(2)
            
            # Sprawdzenie czy serwer się uruchomił
            if self.server_process.poll() is None:
                print(f"✅ Monitor Server uruchomiony na porcie {self.server_port}")
                return True
            else:
                stdout, stderr = self.server_process.communicate()
                print(f"❌ Błąd uruchomienia serwera:")
                print(f"STDOUT: {stdout.decode()}")
                print(f"STDERR: {stderr.decode()}")
                return False
                
        except Exception as e:
            print(f"❌ Nie można uruchomić serwera: {e}")
            return False
    
    def stop_server(self):
        """Zatrzymanie serwera monitora"""
        if self.server_process:
            print("🛑 Zatrzymywanie Monitor Server...")
            self.server_process.terminate()
            try:
                self.server_process.wait(timeout=5)
                print("✅ Monitor Server zatrzymany")
            except subprocess.TimeoutExpired:
                print("⚠️  Wymuszam zatrzymanie serwera...")
                self.server_process.kill()
                self.server_process.wait()
    
    def run_basic_test(self):
        """Test podstawowej funkcjonalności"""
        print("\n" + "="*50)
        print("🧪 TEST 1: Podstawowa funkcjonalność")
        print("="*50)
        
        from monitor_client_step3 import DistributedMonitor
        
        # Test 1: Enter/Exit
        print("\n📝 Test Enter/Exit:")
        monitor1 = DistributedMonitor(f"tcp://localhost:{self.server_port}")
        monitor2 = DistributedMonitor(f"tcp://localhost:{self.server_port}")
        
        try:
            # Klient 1 wchodzi
            monitor1.enter()
            print("✅ Klient 1 wszedł do monitora")
            
            # Klient 2 próbuje wejść (powinien czekać)
            def try_enter():
                print("   Klient 2 próbuje wejść...")
                monitor2.enter()
                print("✅ Klient 2 wszedł do monitora")
                monitor2.exit()
                print("✅ Klient 2 wyszedł z monitora")
            
            thread = threading.Thread(target=try_enter)
            thread.start()
            
            time.sleep(1)  # Klient 2 czeka
            
            # Klient 1 wychodzi
            monitor1.exit()
            print("✅ Klient 1 wyszedł z monitora")
            
            thread.join()
            
        finally:
            monitor1.disconnect()
            monitor2.disconnect()
        
        # Test 2: Wait/Signal
        print("\n📝 Test Wait/Signal:")
        self._test_wait_signal()
        
        print("✅ Test podstawowy zakończony pomyślnie")
    
    def _test_wait_signal(self):
        """Test mechanizmu wait/signal"""
        from monitor_client_step3 import DistributedMonitor
        
        monitor1 = DistributedMonitor(f"tcp://localhost:{self.server_port}")
        monitor2 = DistributedMonitor(f"tcp://localhost:{self.server_port}")
        
        results = {'waiter_woken': False}
        
        def waiter():
            try:
                monitor1.enter()
                print("   Waiter: czekam na warunek 'test_condition'")
                monitor1.wait('test_condition')
                print("✅ Waiter: zostałem obudzony!")
                results['waiter_woken'] = True
                monitor1.exit()
            except Exception as e:
                print(f"❌ Waiter error: {e}")
        
        def signaler():
            time.sleep(0.5)  # Poczekaj aż waiter zacznie czekać
            try:
                monitor2.enter()
                print("   Signaler: sygnalizuję warunek 'test_condition'")
                monitor2.signal('test_condition')
                monitor2.exit()
                print("✅ Signaler: sygnalizacja wysłana")
            except Exception as e:
                print(f"❌ Signaler error: {e}")
        
        waiter_thread = threading.Thread(target=waiter)
        signaler_thread = threading.Thread(target=signaler)
        
        waiter_thread.start()
        signaler_thread.start()
        
        waiter_thread.join()
        signaler_thread.join()
        
        monitor1.disconnect()
        monitor2.disconnect()
        
        if results['waiter_woken']:
            print("✅ Wait/Signal działa poprawnie")
        else:
            print("❌ Wait/Signal nie działa")
    
    def run_producer_consumer_test(self):
        """Test Producer-Consumer"""
        print("\n" + "="*50)
        print("🧪 TEST 2: Producer-Consumer (Bounded Buffer)")
        print("="*50)
        
        try:
            # Uruchomienie testu bounded buffer
            result = subprocess.run([
                sys.executable, 'bounded_buffer_test.py',
                '--buffer-size', '3',
                '--producers', '2',
                '--consumers', '2', 
                '--items-per-producer', '4',
                '--items-per-consumer', '4',
                '--server', f'tcp://localhost:{self.server_port}'
            ], capture_output=True, text=True, timeout=30)
            
            print("📋 Output testu:")
            print(result.stdout)
            
            if result.stderr:
                print("⚠️  Stderr:")
                print(result.stderr)
            
            if result.returncode == 0:
                print("✅ Test Producer-Consumer zakończony pomyślnie")
            else:
                print(f"❌ Test Producer-Consumer zakończony z kodem {result.returncode}")
                
        except subprocess.TimeoutExpired:
            print("❌ Test Producer-Consumer przekroczył timeout")
        except Exception as e:
            print(f"❌ Błąd uruchomienia testu Producer-Consumer: {e}")
    
    def run_broadcast_test(self):
        """Test funkcjonalności broadcast"""
        print("\n" + "="*50)
        print("🧪 TEST 3: Broadcast vs Signal")
        print("="*50)
        
        from monitor_client_step3 import DistributedMonitor
        
        # Test broadcast - obudzi wszystkich czekających
        print("\n📝 Test Broadcast (powinien obudzić 3 waiterów):")
        
        monitors = [DistributedMonitor(f"tcp://localhost:{self.server_port}") for _ in range(4)]
        results = {'woken_count': 0}
        
        def waiter(waiter_id):
            try:
                monitors[waiter_id].enter()
                print(f"   Waiter-{waiter_id}: czekam na 'broadcast_test'")
                monitors[waiter_id].wait('broadcast_test')
                print(f"✅ Waiter-{waiter_id}: obudzony!")
                results['woken_count'] += 1
                monitors[waiter_id].exit()
            except Exception as e:
                print(f"❌ Waiter-{waiter_id} error: {e}")
        
        def broadcaster():
            time.sleep(1)  # Poczekaj aż wszyscy waiterzy zaczną czekać
            try:
                monitors[3].enter()
                print("   Broadcaster: wysyłam broadcast 'broadcast_test'")
                monitors[3].broadcast('broadcast_test')
                monitors[3].exit()
                print("✅ Broadcaster: broadcast wysłany")
            except Exception as e:
                print(f"❌ Broadcaster error: {e}")
        
        # Uruchom 3 waiterów i 1 broadcastera
        threads = []
        for i in range(3):
            thread = threading.Thread(target=waiter, args=(i,))
            threads.append(thread)
            thread.start()
        
        broadcaster_thread = threading.Thread(target=broadcaster)
        threads.append(broadcaster_thread)
        broadcaster_thread.start()
        
        # Czekaj na wszystkie wątki
        for thread in threads:
            thread.join()
        
        # Cleanup
        for monitor in monitors:
            monitor.disconnect()
        
        print(f"📊 Rezultat: {results['woken_count']}/3 waiterów zostało obudzonych")
        if results['woken_count'] == 3:
            print("✅ Test Broadcast zakończony pomyślnie")
        else:
            print("❌ Test Broadcast nie obudził wszystkich waiterów")
    
    def run_stress_test(self):
        """Test obciążeniowy"""
        print("\n" + "="*50)
        print("🧪 TEST 4: Stress Test")
        print("="*50)
        
        try:
            # Uruchom duży test bounded buffer
            result = subprocess.run([
                sys.executable, 'bounded_buffer_test.py',
                '--buffer-size', '5',
                '--producers', '5',
                '--consumers', '5',
                '--items-per-producer', '10',
                '--items-per-consumer', '10',
                '--server', f'tcp://localhost:{self.server_port}'
            ], capture_output=True, text=True, timeout=60)
            
            if result.returncode == 0:
                print("✅ Stress Test zakończony pomyślnie")
                # Pokaż tylko ostatnie linie outputu
                lines = result.stdout.strip().split('\n')
                print("📋 Ostatnie linie:")
                for line in lines[-10:]:
                    print(f"   {line}")
            else:
                print(f"❌ Stress Test zakończony z kodem {result.returncode}")
                print("📋 Stderr:")
                print(result.stderr)
                
        except subprocess.TimeoutExpired:
            print("❌ Stress Test przekroczył timeout (60s)")
        except Exception as e:
            print(f"❌ Błąd Stress Test: {e}")
    
    def run_all_tests(self):
        """Uruchomienie wszystkich testów"""
        print("🎯 DISTRIBUTED MONITOR - ETAP 3 - TESTY")
        print("Testowanie condition variables (wait/signal/broadcast)")
        
        # Sprawdzenie czy pliki istnieją
        required_files = [
            'monitor_server_step3.py',
            'monitor_client_step3.py', 
            'bounded_buffer_test.py'
        ]
        
        for file in required_files:
            if not Path(file).exists():
                print(f"❌ Brak pliku: {file}")
                return False
        
        # Uruchomienie serwera
        if not self.start_server():
            return False
        
        try:
            # Uruchomienie testów
            self.run_basic_test()
            self.run_producer_consumer_test()
            self.run_broadcast_test()
            self.run_stress_test()
            
            print("\n" + "="*50)
            print("🎉 WSZYSTKIE TESTY ZAKOŃCZONE")
            print("="*50)
            
            return True
            
        except Exception as e:
            print(f"❌ Błąd podczas testów: {e}")
            return False
            
        finally:
            self.stop_server()

def main():
    """Główna funkcja"""
    runner = TestRunner()
    
    try:
        success = runner.run_all_tests()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        print("\n⚠️  Testy przerwane przez użytkownika")
        runner.stop_server()
        sys.exit(1)

if __name__ == "__main__":
    main()