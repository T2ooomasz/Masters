# Projekt 4: Narzędzie sterujące mocą sygnału karty bezprzewodowej (wersja C)

## Nazwa i opis projektu

**Nazwa:** `Wireless Power Control Tool (C Edition)`

**Opis:**
Narzędzie wiersza poleceń napisane w języku C, które pozwala na zarządzanie mocą nadawania (Tx Power) bezprzewodowych kart sieciowych w systemie operacyjnym GNU/Linux. Program automatycznie wykrywa dostępne interfejsy Wi-Fi, wyświetla ich aktualną moc i umożliwia użytkownikowi ustawienie nowej wartości mocy dla wybranego interfejsu.

Projekt jest implementacją w C, która wykorzystuje standardowe funkcje biblioteczne (`popen`, `system`) do interakcji z narzędziami systemowymi `iw` oraz `ip`.

## Zawartość plików źródłowych i zasady ich kompilacji

### Pliki źródłowe:

* `wireless_power_tool.c`: Główny i jedyny plik źródłowy projektu. Zawiera całą logikę aplikacji w języku C.

### Zasady kompilacji:

Projekt należy skompilować przy użyciu kompilatora C, np. `gcc`.

1.  **Otwórz terminal.**
2.  **Przejdź do katalogu z plikiem `wireless_power_tool.c`.**
3.  **Wykonaj polecenie kompilacji:**

    ```bash
    gcc wireless_power_tool.c -o wireless_power_tool
    ```

    Polecenie to stworzy plik wykonywalny o nazwie `wireless_power_tool`.

### Zależności systemowe:

Podobnie jak wersja w Pythonie, narzędzie do poprawnego działania wymaga zainstalowanych w systemie pakietów:
* `iw`: Narzędzie do konfiguracji urządzeń bezprzewodowych.
* `iproute2` (dostarczający polecenie `ip`): Narzędzie do zarządzania interfejsami sieciowymi.

W większości dystrybucji Linuksa (np. Debian, Ubuntu, Arch Linux) pakiety te są dostępne w standardowych repozytoriach.
Można je zainstalować poleceniem:
```bash
# Dla systemów bazujących na Debianie/Ubuntu
sudo apt-get update && sudo apt-get install iw iproute2

# Dla systemów bazujących na Arch Linux
sudo pacman -S iw iproute2