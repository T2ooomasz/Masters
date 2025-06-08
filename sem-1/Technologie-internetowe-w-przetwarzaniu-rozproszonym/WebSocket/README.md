##  Jak uruchomić aplikację?
* Zainstaluj wymagane biblioteki w Pythonie:
``bash``
`pip install websockets`

* Uruchom serwer:
* * Zapisz kod serwera jako `server.py`.

* * W terminalu wykonaj:
bash

`python3 server.py`

* Uruchom klienta:
* * Zapisz kod klienta jako `index.html`.

* * Otwórz plik w przeglądarce (np. Firefox/Chrome).

* * Otwórz drugą kartę lub przeglądarkę z tym samym plikiem, aby zasymulować dwóch graczy.

* Sterowanie:
* * Gracz 1 i Gracz 2 używają strzałek ↑ (góra) i ↓ (dół) do poruszania paletkami.

## Jak działa aplikacja?
* Serwer:
* * Tworzy instancje gry dla dwóch graczy.

* * Aktualizuje stan gry (pozycja piłki, paletek, punkty) w pętli 60 FPS.

* * Wysyła stan gry do klientów w formacie binarnym za pomocą WebSocket.

* Klient:
* * Łączy się z serwerem i dołącza do gry, wysyłając swoje `playerId`.

* * Odbiera stan gry i renderuje go na `<canvas>`.

* * ysyła ruchy paletek (góra/dół) do serwera na podstawie naciśnięć klawiszy.

## Funkcjonalności
* Komunikacja asynchroniczna: WebSocket umożliwia dwukierunkową komunikację w czasie rzeczywistym.

* Obsługa wielu gier: Serwer może zarządzać wieloma instancjami gry równocześnie.

* Optymalizacja: Użycie binarnych komunikatów zmniejsza rozmiar przesyłanych danych.

* Odporność na odświeżanie: playerId w localStorage pozwala graczowi wrócić do gry po odświeżeniu strony.

