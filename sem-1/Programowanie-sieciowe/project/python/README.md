# Projekt 4: Narzędzie sterujące mocą sygnału karty bezprzewodowej

## Nazwa i opis projektu

**Nazwa:** `Wireless Power Control Tool`

**Opis:**
Narzędzie wiersza poleceń napisane w języku Python, które pozwala na zarządzanie mocą nadawania (Tx Power) bezprzewodowych kart sieciowych w systemie operacyjnym GNU/Linux. Program automatycznie wykrywa dostępne interfejsy Wi-Fi, wyświetla ich aktualną moc i umożliwia użytkownikowi ustawienie nowej wartości mocy dla wybranego interfejsu.

Projekt wykorzystuje standardowe narzędzia systemowe takie jak `iw` i `ip`, opakowując ich funkcjonalność w prosty i interaktywny interfejs.

## Zawartość plików źródłowych i zasady ich kompilacji

### Pliki źródłowe:

* `wireless_power_tool.py`: Główny i jedyny plik źródłowy projektu. Zawiera całą logikę aplikacji.

### Zasady kompilacji:

Projekt jest skryptem w języku Python i nie wymaga kompilacji. Do jego uruchomienia potrzebny jest interpreter Pythona w wersji 3.x.

### Zależności systemowe:

Narzędzie do poprawnego działania wymaga zainstalowanych w systemie pakietów:
* `iw`: Narzędzie do konfiguracji urządzeń bezprzewodowych.
* `iproute2` (dostarczający polecenie `ip`): Narzędzie do zarządzania interfejsami sieciowymi.

W większości dystrybucji Linuksa (np. Debian, Ubuntu, Arch Linux) pakiety te są dostępne w standardowych repozytoriach.
Można je zainstalować poleceniem:
```bash
# Dla systemów bazujących na Debianie/Ubuntu
sudo apt-get update && sudo apt-get install iw iproute2

# Dla systemów bazujących na Arch Linux
sudo pacman -S iw iproute2