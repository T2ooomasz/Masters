#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
sudo python3 wireless_power_tool.py

$ sudo python3 wireless_power_tool.py
--- Narzędzie do sterowania mocą karty bezprzewodowej ---

Dostępne interfejsy bezprzewodowe:
1. wlan0 (Aktualna moc: 20.00 dBm)

Wybierz interfejs (podaj numer): 1
Podaj nową moc nadawania dla 'wlan0' (w dBm): 15

Konfigurowanie interfejsu 'wlan0'...
1. Wyłączanie interfejsu 'wlan0'...
2. Ustawianie mocy nadawania na 15 dBm...
3. Włączanie interfejsu 'wlan0'...

Konfiguracja zakończona pomyślnie!

Sprawdzanie nowej konfiguracji...
Nowa moc dla interfejsu 'wlan0': 15.00 dBm
'''
import subprocess
import sys
import re

def run_command(command):
    """
    Wykonuje polecenie systemowe i zwraca jego wynik.
    """
    try:
        process = subprocess.Popen(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, shell=True)
        stdout, stderr = process.communicate()
        if process.returncode != 0:
            print(f"Błąd podczas wykonywania polecenia: {command}")
            print(f"Stderr: {stderr.strip()}")
            return None
        return stdout.strip()
    except FileNotFoundError:
        print(f"Błąd: Polecenie '{command.split()[0]}' nie zostało znalezione. Upewnij się, że narzędzia 'iw' oraz 'ip' są zainstalowane.")
        sys.exit(1)
    except Exception as e:
        print(f"Wystąpił nieoczekiwany błąd: {e}")
        sys.exit(1)

def get_wireless_interfaces():
    """
    Pobiera listę dostępnych interfejsów bezprzewodowych.
    """
    output = run_command("iw dev")
    if not output:
        return []
    
    # Użycie wyrażenia regularnego do znalezienia nazw interfejsów
    interfaces = re.findall(r'Interface\s+(\w+)', output)
    return interfaces

def get_current_tx_power(interface):
    """
    Pobiera aktualną moc nadawania dla danego interfejsu.
    """
    output = run_command(f"iw dev {interface} link")
    if not output or "Not connected" in output:
        # Jeśli interfejs nie jest połączony, spróbuj innej komendy
        output = run_command(f"iw dev {interface} info")

    if not output:
        return "Nie można odczytać mocy"

    # Wyszukanie linii zawierającej 'txpower'
    match = re.search(r'txpower\s+([\d\.]+\s+dBm)', output)
    if match:
        return match.group(1)
    return "Nieznana"

def set_tx_power(interface, power_dbm):
    """
    Ustawia moc nadawania dla wybranego interfejsu.
    """
    print(f"\nKonfigurowanie interfejsu '{interface}'...")

    # 1. Wyłączenie interfejsu
    print(f"1. Wyłączanie interfejsu '{interface}'...")
    if run_command(f"sudo ip link set {interface} down") is None:
        return False

    # 2. Ustawienie nowej mocy
    print(f"2. Ustawianie mocy nadawania na {power_dbm} dBm...")
    # 'iw' oczekuje wartości w mBm (decybelomiliwatach), ale nowsze wersje przyjmują też dBm
    if run_command(f"sudo iw dev {interface} set txpower fixed {power_dbm}mBm") is None:
         # Próba alternatywnej komendy, jeśli pierwsza zawiedzie
        if run_command(f"sudo iw dev {interface} set txpower fixed {power_dbm}") is None:
            print("Nie udało się ustawić mocy. Próbuję przywrócić interfejs do stanu początkowego.")
            run_command(f"sudo ip link set {interface} up")
            return False

    # 3. Włączenie interfejsu
    print(f"3. Włączanie interfejsu '{interface}'...")
    if run_command(f"sudo ip link set {interface} up") is None:
        return False
    
    print("\nKonfiguracja zakończona pomyślnie!")
    return True


def main():
    """
    Główna funkcja programu.
    """
    if os.geteuid() != 0:
        print("Ten skrypt wymaga uprawnień administratora (root) do zmiany ustawień karty sieciowej.")
        print("Uruchom go używając 'sudo python3 wireless_power_tool.py'")
        sys.exit(1)

    print("--- Narzędzie do sterowania mocą karty bezprzewodowej ---")
    
    interfaces = get_wireless_interfaces()
    if not interfaces:
        print("\nNie znaleziono żadnych interfejsów bezprzewodowych.")
        print("Upewnij się, że karta Wi-Fi jest włączona i sterowniki są poprawnie zainstalowane.")
        sys.exit(1)

    print("\nDostępne interfejsy bezprzewodowe:")
    for i, iface in enumerate(interfaces, 1):
        power = get_current_tx_power(iface)
        print(f"{i}. {iface} (Aktualna moc: {power})")

    try:
        choice = int(input("\nWybierz interfejs (podaj numer): "))
        if not 1 <= choice <= len(interfaces):
            print("Nieprawidłowy wybór.")
            sys.exit(1)
        selected_interface = interfaces[choice - 1]

        power = int(input(f"Podaj nową moc nadawania dla '{selected_interface}' (w dBm): "))
        
        set_tx_power(selected_interface, power)

        print("\nSprawdzanie nowej konfiguracji...")
        new_power = get_current_tx_power(selected_interface)
        print(f"Nowa moc dla interfejsu '{selected_interface}': {new_power}")

    except ValueError:
        print("Nieprawidłowa wartość. Proszę podać liczbę.")
    except KeyboardInterrupt:
        print("\nPrzerwano działanie programu.")
        sys.exit(0)

if __name__ == "__main__":
    import os
    main()