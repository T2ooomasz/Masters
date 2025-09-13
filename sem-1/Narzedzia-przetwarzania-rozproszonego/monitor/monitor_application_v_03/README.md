# Implementacja Rozproszonego Monitora w Pythonie z ZeroMQ

Projekt ten przedstawia implementację **rozproszonego monitora**, czyli mechanizmu synchronizacji dla procesów działających na różnych maszynach lub w oddzielnych przestrzeniach adresowych. Rozwiązanie bazuje na architekturze klient-serwer i wykorzystuje bibliotekę **ZeroMQ (ØMQ)** do komunikacji.

Głównym celem jest dostarczenie interfejsu programistycznego, który jest zbliżony do klasycznych monitorów znanych z programowania współbieżnego (np. z języka Java czy z modułu `threading` w Pythonie), ale działającego w środowisku rozproszonym.

## Architektura

System składa się z dwóch głównych komponentów: centralnego serwera monitora oraz biblioteki klienckiej, której używają aplikacje.

### 1. Serwer Monitora (`monitor_server.py`)

- **Centralny punkt synchronizacji**: Serwer jest pojedynczym, jednowątkowym procesem, który zarządza stanem wszystkich monitorów. Dzięki jednowątkowości operacje na danym monitorze są z natury atomowe, co eliminuje potrzebę stosowania dodatkowych blokad po stronie serwera.
- **Zarządzanie wieloma monitorami**: Serwer potrafi zarządzać wieloma niezależnymi monitorami. Każdy monitor jest identyfikowany przez unikalną nazwę (string). Aplikacje klienckie, używając tej samej nazwy, odnoszą się do tego samego, współdzielonego stanu.
- **Komunikacja**: Serwer używa gniazda `REP` (Reply) z ZeroMQ, co oznacza, że działa w pętli "oczekuj na żądanie -> przetwórz -> wyślij odpowiedź".

### 2. Klient Monitora (`monitor_client.py`)

- **Abstrakcja dla programisty**: Biblioteka kliencka dostarcza klasę `DistributedMonitor`, która ukrywa całą złożoność komunikacji sieciowej. Programista używa metod `enter()`, `exit()`, `wait()`, `signal()`, `broadcast()` tak, jakby operował na lokalnym obiekcie.
- **Wsparcie dla `with` statement**: Klient implementuje metody `__enter__` i `__exit__`, co pozwala na wygodne i bezpieczne użycie monitora w bloku `with`, gwarantując zwolnienie blokady nawet w przypadku wystąpienia wyjątku.
- **Komunikacja**: Klient używa gniazda `REQ` (Request) z ZeroMQ do wysyłania żądań do serwera.

## Intuicyjne API: Przykłady i Porównanie

API `DistributedMonitor` zostało zaprojektowane tak, aby było niemal identyczne z najpopularniejszym wbudowanym monitorem w Pythonie: `threading.Condition`. Dzięki temu programiści mogą łatwo przenieść swoją wiedzę i intuicje z programowania wielowątkowego do środowiska rozproszonego.

Poniższe przykłady ilustrują, jak rozwiązać ten sam prosty problem (synchronizacja dostępu do współdzielonej listy) przy użyciu obu narzędzi.

| `threading.Condition` (Lokalnie, w jednym procesie) | `DistributedMonitor` (Rozproszony, między procesami) |
| --------------------------------------------------- | ------------------------------------------------------ |
| ```python                                           | ```python                                              |
| import threading                                    | from monitor_client import DistributedMonitor          |
|                                                     | # Potrzebny jest też obiekt współdzielony między procesami |
| # Współdzielone zasoby                                | # (np. z multiprocessing.Manager)                      |
| items = []                                          | manager = Manager()                                    |
| condition = threading.Condition()                   | items = manager.list()                                 |
|                                                     |                                                        |
| def worker():                                       | # Inicjalizacja w procesie roboczym                    |
|     with condition:                                 | monitor = DistributedMonitor(                          |
|         while len(items) == 0:                      |     monitor_name="my_shared_list",                     |
|             print("Czekam na item...")              |     server_address="tcp://localhost:5555"              |
|             condition.wait()                        | )                                                      |
|                                                     |                                                        |
|         item = items.pop(0)                         | def worker():                                          |
|         print(f"Pobrano: {item}")                   |     with monitor:                                      |
|                                                     |         while len(items) == 0:                         |
| def generator():                                    |             print("Czekam na item...")                 |
|     with condition:                                 |             monitor.wait("item_available")             |
|         items.append("jakiś item")                  |                                                        |
|         print("Dodano item, powiadamiam...")        |         item = items.pop(0)                            |
|         condition.notify()                          |         print(f"Pobrano: {item}")                      |
| ```                                                 |                                                        |
|                                                     | def generator():                                       |
|                                                     |     # ... inicjalizacja monitora ...                   |
|                                                     |     with monitor:                                      |
|                                                     |         items.append("jakiś item")                     |
|                                                     |         print("Dodano item, powiadamiam...")           |
|                                                     |         monitor.signal("item_available")               |
|                                                     | ```                                                    |

**Kluczowe obserwacje:**

- **Logika jest identyczna**: Zwróć uwagę, że kod wewnątrz bloków `with` jest praktycznie taki sam. Zamiast `condition.wait()` używamy `monitor.wait("nazwa_warunku")`, a zamiast `condition.notify()` – `monitor.signal("nazwa_warunku")`.
- **Inicjalizacja**: Główna różnica polega na inicjalizacji. `DistributedMonitor` wymaga podania unikalnej **nazwy monitora** (która łączy procesy) oraz **adresu serwera**.
- **Zmienne warunkowe**: Rozproszony monitor pozwala na tworzenie wielu zmiennych warunkowych pod jedną blokadą, identyfikowanych przez nazwy (np. `"item_available"`, `"buffer_not_full"`), co jest dodatkową zaletą.

Dzięki takiemu podejściu, jedyną nową koncepcją dla programisty jest świadomość, że synchronizacja odbywa się przez sieć, a nie w pamięci jednego procesu.

### Protokół Komunikacyjny

Komunikacja między klientem a serwerem odbywa się za pomocą komunikatów w formacie **JSON**. Klient wysyła żądanie zawierające akcję do wykonania, nazwę monitora i unikalne ID procesu.

**Podstawowe akcje:**
- `ENTER`: Prośba o wejście do sekcji krytycznej (uzyskanie muteksu).
- `EXIT`: Wyjście z sekcji krytycznej (zwolnienie muteksu).
- `WAIT`: Oczekiwanie na zmiennej warunkowej (zwalnia muteks i usypia proces).
- `SIGNAL`: Budzi jeden proces oczekujący na zmiennej warunkowej.
- `BROADCAST`: Budzi wszystkie procesy oczekujące na zmiennej warunkowej.

**Mechanizm Pollingu:**
Aby zaimplementować blokujące wywołania `enter()` i `wait()` po stronie klienta bez blokowania serwera, zastosowano mechanizm **pollingu**.
1.  Gdy klient chce wejść do monitora, który jest zajęty, serwer odpowiada statusem `queued`. Klient wchodzi wtedy w pętlę, w której co pewien czas wysyła żądanie `CHECK_GRANT`, aż serwer odpowie `granted`.
2.  Podobnie, po wywołaniu `wait()`, klient cyklicznie wysyła `CHECK_AWAKENED`, aby sprawdzić, czy został obudzony i czy odzyskał muteks.

Takie podejście upraszcza logikę serwera, który pozostaje bezstanowy w kontekście długotrwałych operacji i nie musi zarządzać aktywnymi połączeniami z zablokowanymi klientami.

## Przykładowe Użycie: Problem Producenta i Konsumenta

W projekcie znajduje się klasyczna implementacja problemu producenta i konsumenta z ograniczonym buforem.

- **`bounded_buffer.py`**: Definiuje klasę `BoundedBuffer`, która używa `DistributedMonitor` do synchronizacji dostępu do współdzielonego bufora. Implementuje metody `put()` i `get()`, które blokują, gdy bufor jest odpowiednio pełny lub pusty.
- **`producer_consumer.py`**: Uruchamia symulację z wieloma procesami producentów i konsumentów, które komunikują się poprzez `BoundedBuffer`. Każdy proces tworzy własną instancję klienta monitora, ale wszystkie odwołują się do tej samej nazwy monitora na serwerze, co zapewnia poprawną synchronizację.

## Testy Wydajności

Skrypt `performance_test.py` służy do przeprowadzania testów wydajnościowych i obciążeniowych. Uruchamia on symulację producent-konsument w różnych konfiguracjach (zmienna liczba producentów, konsumentów, rozmiar bufora) i mierzy kluczowe wskaźniki:
- **Całkowity czas wykonania**
- **Przepustowość** (liczba operacji na sekundę)
- **Średnia latencja** pojedynczej operacji (`put`/`get`)

Wyniki testów są zapisywane w pliku `performance_results_distributed.csv`, co pozwala na dalszą analizę i porównanie wydajności w różnych warunkach.

## Jak Uruchomić

1.  **Uruchom serwer monitora**:
    ```bash
    python monitor_server.py
    ```
    Serwer domyślnie nasłuchuje na porcie `5555`.

2.  **Uruchom aplikację producenta-konsumenta** (w nowym terminalu):
    ```bash
    python producer_consumer.py
    ```
    Skrypt uruchomi kilka procesów, które będą komunikować się z serwerem w celu synchronizacji dostępu do bufora.

3.  **Uruchom testy wydajności** (opcjonalnie):
    ```bash
    python performance_test.py
    ```
    Skrypt przeprowadzi serię testów i zapisze wyniki w pliku CSV.
