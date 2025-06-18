# Projekt: Automatyczny Menedżer Mocy Karty Bezprzewodowej (wersja C)

## Nazwa i opis projektu

**Nazwa:** `auto_power_manager`

**Opis:**
Zaawansowana wersja narzędzia do sterowania mocą karty Wi-Fi, napisana w języku C. Program działa jako usługa w tle (demon), która w sposób ciągły monitoruje siłę sygnału aktywnego połączenia bezprzewodowego. Na podstawie zdefiniowanych przez użytkownika progów, narzędzie automatycznie zwiększa moc nadawania, gdy sygnał słabnie, lub zmniejsza ją, gdy sygnał jest bardzo silny, optymalizując w ten sposób zużycie energii przy jednoczesnym zachowaniu stabilności połączenia.

Zmiany mocy są dokonywane "w locie", bez konieczności restartowania interfejsu, co zapewnia nieprzerwane działanie sieci.

## Zasady kompilacji

Projekt należy skompilować przy użyciu kompilatora C, np. `gcc`.

1.  **Otwórz terminal.**
2.  **Przejdź do katalogu z plikiem `auto_power_manager.c`.**
3.  **Wykonaj polecenie kompilacji:**

    ```bash
    gcc auto_power_manager.c -o auto_power_manager
    ```

    Polecenie to stworzy plik wykonywalny o nazwie `auto_power_manager`.

### Zależności systemowe:

Narzędzie do poprawnego działania wymaga zainstalowanych w systemie pakietów:
* `iw`: Narzędzie do konfiguracji urządzeń bezprzewodowych.
* `iproute2` (dostarczający polecenie `ip`): Narzędzie do zarządzania interfejsami sieciowymi.

## Sposób uruchomienia programu

Program przyjmuje parametry konfiguracyjne jako argumenty linii poleceń. Wymaga uprawnień administratora (`sudo`).

### Składnia:
```bash
sudo ./auto_power_manager <interfejs> <min_sygnal> <opt_sygnal> <min_moc> <max_moc> <interwal_s>