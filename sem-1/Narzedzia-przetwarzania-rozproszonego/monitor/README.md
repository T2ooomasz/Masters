# Podsumowanie Projektu: Rozproszony Monitor z Warunkami Zmiennymi

## 1. Wprowadzenie i Cel Projektu

Celem tego projektu było zaprojektowanie i zaimplementowanie w pełni funkcjonalnego **rozproszonego monitora z obsługą warunków zmiennych**. Monitor ten naśladuje klasyczne mechanizmy synchronizacji znane z programowania współbieżnego (takie jak monitory Hoare'a czy Brincha Hansena), ale został przystosowany do działania w środowisku rozproszonym, gdzie wiele procesów (potencjalnie na różnych maszynach) musi koordynować dostęp do współdzielonych zasobów.

Projekt przeszedł przez kilka etapów, począwszy od podstawowej komunikacji z ZeroMQ, przez implementację wzorca REQ-REP dla atomowych operacji, aż po rozszerzenie o warunki zmienne i budowę przykładowej aplikacji (`BoundedBuffer`) demonstrującej jego użycie.

## 2. Architektura Systemu

System opiera się na architekturze klient-serwer:

### 2.1. Serwer Monitora (`monitor_server.py`)

*   **Centralny Punkt Synchronizacji**: Serwer działa jako autorytatywne źródło stanu dla wszystkich zarządzanych monitorów. Przechowuje informacje o tym, który proces posiada muteks (wyłączny dostęp) do danego monitora, oraz zarządza kolejkami procesów oczekujących na wejście do monitora lub na spełnienie określonych warunków zmiennych.
*   **Komunikacja**: Wykorzystuje bibliotekę **ZeroMQ (ØMQ)** z wzorcem komunikacyjnym **REQ-REP**. Ten wzorzec zapewnia, że serwer obsługuje jedno żądanie od klienta w danym momencie, co gwarantuje atomowość operacji na poziomie serwera.
*   **Zarządzanie Stanem Monitora**:
    *   **Muteks**: Dla każdego nazwanego monitora serwer śledzi, który `process_id` klienta jest aktualnym właścicielem muteksu.
    *   **Kolejka Wejścia (`enter_queue`)**: Utrzymuje kolejkę FIFO procesów, które zażądały wejścia do monitora (`enter()`), gdy był on zajęty, lub które zostały obudzone z warunku (`wait()`) i muszą ponownie ubiegać się o muteks.
    *   **Kolejki Warunków Zmiennych (`condition_queues`)**: Dla każdego monitora i każdej nazwanej zmiennej warunkowej serwer utrzymuje oddzielną kolejkę FIFO procesów oczekujących na ten warunek.
*   **Obsługa Operacji**: Serwer implementuje logikę dla następujących operacji inicjowanych przez klienta:
    *   `ENTER`: Próba uzyskania muteksu.
    *   `EXIT`: Zwolnienie muteksu.
    *   `WAIT`: Zwolnienie muteksu i oczekiwanie na określony warunek.
    *   `SIGNAL`: Obudzenie jednego procesu oczekującego na określony warunek.
    *   `BROADCAST`: Obudzenie wszystkich procesów oczekujących na określony warunek.
    *   `CHECK_GRANT` / `CHECK_AWAKENED`: Operacje pomocnicze dla strategii pollingu po stronie klienta.

### 2.2. Klient Monitora (`monitor_client.py`)

*   **Interfejs Programistyczny (API)**: Udostępnia programistom API, które jest intuicyjne i bardzo podobne do standardowych bibliotek monitorów (np. `threading.Condition` w Pythonie).
*   **Identyfikacja**: Każda instancja `DistributedMonitor` generuje unikalny `process_id` (łączący UUID z identyfikatorem wątku), który jest używany do identyfikacji klienta przez serwer.
*   **Komunikacja z Serwerem**: Wysyła żądania w formacie JSON do serwera i odbiera odpowiedzi, również w JSON.
*   **Strategia Pollingu**: Dla operacji, które mogą blokować (jak `enter()` gdy monitor jest zajęty, lub `wait()` do momentu zasygnalizowania warunku i odzyskania muteksu), klient implementuje strategię pollingu. Po otrzymaniu od serwera informacji, że żądanie zostało zakolejkowane lub proces czeka na warunek, klient okresowo wysyła żądania `CHECK_GRANT` lub `CHECK_AWAKENED` do serwera, aż do momentu, gdy operacja zostanie pomyślnie zakończona.
*   **Zarządzanie Kontekstem (`with` statement)**: Klasa `DistributedMonitor` implementuje metody `__enter__` i `__exit__`, co pozwala na wygodne użycie monitora w bloku `with`, automatycznie zarządzając operacjami `enter()` i `exit()`.
*   **Zarządzanie Połączeniem**: Klient zarządza swoim socketem ZMQ, w tym ponownym nawiązywaniem połączenia w przypadku niektórych błędów. Metoda `close()` pozwala na jawne zwolnienie zasobów.

## 3. Kluczowe Funkcjonalności i Sposób Użycia

### 3.1. Podstawowe Operacje

*   **`enter()`**: Uzyskuje wyłączny dostęp do monitora (muteks). Blokuje, jeśli monitor jest zajęty.
*   **`exit()`**: Zwalnia muteks, pozwalając innym oczekującym procesom na wejście.
*   **`wait(condition_name: str)`**: Musi być wywołane, gdy proces posiada muteks. Proces zwalnia muteks i zostaje umieszczony w kolejce oczekujących na warunek `condition_name`. Po obudzeniu, proces automatycznie próbuje ponownie uzyskać muteks przed kontynuacją.
*   **`signal(condition_name: str)`**: Musi być wywołane, gdy proces posiada muteks. Budzi jeden proces (jeśli istnieje) oczekujący na warunek `condition_name`.
*   **`broadcast(condition_name: str)`**: Musi być wywołane, gdy proces posiada muteks. Budzi wszystkie procesy oczekujące na warunek `condition_name`.

### 3.2. Jak Używać Monitora (Przykład z `BoundedBuffer`)

1.  **Uruchom Serwer**:
    ```bash
    python monitor_server.py
    ```

2.  **W Kodzie Aplikacji Klienckiej**:
    Załóżmy, że mamy klasę `BoundedBuffer` (jak w `bounded_buffer.py`), która używa rozproszonego monitora do synchronizacji operacji `put` i `get` na współdzielonym buforze.

    *   **Współdzielenie Danych Bufora**: Ponieważ procesy nie współdzielą pamięci bezpośrednio w standardowy sposób, faktyczna kolekcja przechowująca elementy bufora musi być obiektem współdzielonym między procesami. W przykładzie `producer_consumer.py` użyto do tego `multiprocessing.Manager().list()`.

    *   **Inicjalizacja w Procesach Potomnych**: Każdy proces (producent lub konsument) musi utworzyć własną instancję `DistributedMonitor` oraz `BoundedBuffer`. Kluczowe jest, aby:
        *   Wszystkie instancje `DistributedMonitor` odwołujące się do tego samego logicznego monitora używały tej samej `monitor_name` i `server_address`.
        *   Wszystkie instancje `BoundedBuffer` operowały na tym samym obiekcie proxy współdzielonej kolekcji danych (np. przekazanym `manager.list()`).

    Przykład (fragment z funkcji workera w `producer_consumer.py`):
    ```python
    # W funkcji docelowej procesu (np. producer_process)
    import multiprocessing as mp
    from monitor_client import DistributedMonitor
    from bounded_buffer import BoundedBuffer # Załóżmy, że ta wersja przyjmuje shared_proxy

    # Parametry przekazane do procesu
    monitor_name = "my_shared_buffer_monitor"
    server_address = "tcp://localhost:5555"
    buffer_capacity = 10
    
    # shared_buffer_proxy jest tworzone przez mp.Manager().list() w procesie głównym
    # i przekazywane jako argument do tej funkcji workera.

    # Każdy proces tworzy swojego klienta monitora
    monitor_client = DistributedMonitor(monitor_name, server_address)
    
    # Każdy proces tworzy swoją instancję BoundedBuffer,
    # ale operuje na współdzielonym 'shared_buffer_proxy'
    bb_instance = BoundedBuffer(buffer_capacity, monitor_client, shared_buffer_proxy)

    # Użycie bufora
    # bb_instance.put(item)
    # item = bb_instance.get()
    
    # Na końcu pracy procesu
    monitor_client.close()
    ```

    *   **Logika `BoundedBuffer`**:
        ```python
        class BoundedBuffer:
            def __init__(self, capacity, monitor, shared_buffer_collection_proxy):
                self.capacity = capacity
                self.monitor = monitor
                self.buffer_proxy = shared_buffer_collection_proxy # np. manager.list()
                self.COND_NOT_FULL = "not_full"
                self.COND_NOT_EMPTY = "not_empty"

            def put(self, item):
                with self.monitor: # Automatyczne enter() i exit()
                    while len(self.buffer_proxy) >= self.capacity:
                        self.monitor.wait(self.COND_NOT_FULL)
                    self.buffer_proxy.append(item)
                    self.monitor.signal(self.COND_NOT_EMPTY)
            
            def get(self):
                with self.monitor:
                    while len(self.buffer_proxy) == 0:
                        self.monitor.wait(self.COND_NOT_EMPTY)
                    item = self.buffer_proxy.pop(0) # FIFO
                    self.monitor.signal(self.COND_NOT_FULL)
                    return item
        ```

## 4. Testowanie

Projekt został poddany kilku rodzajom testów:

*   **Testy Jednostkowe (`test_monitor.py`)**: Weryfikowały podstawowe operacje monitora, wzajemne wykluczanie oraz poprawność działania warunków zmiennych (`wait`, `signal`, `broadcast`) w scenariuszach wieloprocesowych.
*   **Test Aplikacji (`producer_consumer.py`)**: Demonstrował praktyczne użycie monitora w klasycznym problemie producenta-konsumenta z ograniczonym buforem, sprawdzając poprawność funkcjonalną.
*   **Test Porównawczy (`local_vs_distributed.py`)**: Porównywał wydajność implementacji `BoundedBuffer` z rozproszonym monitorem oraz z lokalnym monitorem (wykorzystującym `multiprocessing.Lock` i `multiprocessing.Condition` dla uczciwego porównania w środowisku wieloprocesowym). Jak oczekiwano, monitor rozproszony wprowadza narzut związany z komunikacją sieciową.
*   **Testy Wydajności i Stresu (`performance_test.py`)**: Umożliwiały badanie zachowania systemu pod różnym obciążeniem (zmienna liczba producentów/konsumentów, liczba elementów, pojemność bufora), mierząc całkowity czas wykonania, liczbę operacji na sekundę oraz średnią latencję operacji. Wyniki były zapisywane do pliku CSV dla dalszej analizy.

## 5. Mocne Strony i Ograniczenia

### Mocne Strony

*   **Poprawność Funkcjonalna**: Zaimplementowany monitor poprawnie realizuje semantykę operacji `enter`/`exit` oraz `wait`/`signal`/`broadcast` dla warunków zmiennych.
*   **Centralizacja Stanu**: Upraszcza logikę i eliminuje problemy spójności stanu między klientami.
*   **Atomowość Operacji Serwera**: Wzorzec REQ-REP zapewnia, że operacje modyfikujące stan monitora na serwerze są wykonywane atomowo.
*   **Przejrzyste API Klienta**: Interfejs jest zbliżony do standardowych monitorów, co ułatwia adaptację.
*   **Obsługa Wielu Monitorów**: Serwer może zarządzać wieloma niezależnymi, nazwanymi monitorami jednocześnie.

### Ograniczenia i Potencjalne Ulepszenia

*   **Pojedynczy Punkt Awarii (SPOF)**: Awaria serwera monitora unieruchamia cały system synchronizacji. Rozwiązaniem mogłaby być replikacja serwera lub użycie bardziej złożonych protokołów konsensusu.
*   **Wydajność**: Komunikacja sieciowa (nawet lokalna przez `localhost`) wprowadza znaczący narzut w porównaniu do mechanizmów synchronizacji wewnątrzprocesowej lub międzyprocesowej na tej samej maszynie bez udziału sieci.
*   **Strategia Pollingu Klienta**: Chociaż prosta w implementacji, może prowadzić do nieoptymalnego wykorzystania zasobów i wprowadzać dodatkowe opóźnienia. Rozwiązania oparte na asynchronicznych powiadomieniach (np. ZMQ PUB/SUB) mogłyby być bardziej wydajne, ale skomplikowałyby implementację.
*   **Brak Gwarancji Sprawiedliwości (Fairness)**: Mimo użycia kolejek FIFO, skomplikowane interakcje sieciowe i timing mogą wpłynąć na rzeczywistą kolejność obsługi procesów.
*   **Zarządzanie Zasobami Serwera**: Długo działający serwer z wieloma dynamicznie tworzonymi i porzucanymi monitorami mógłby wymagać mechanizmów czyszczenia nieużywanych stanów monitorów, aby zapobiec wyciekom pamięci (obecnie stany są tworzone na żądanie i pozostają).

## 6. Wnioski Końcowe

Projekt z powodzeniem demonstruje, jak można zbudować rozproszony mechanizm synchronizacji typu monitor, wykorzystując stosunkowo proste narzędzia jak ZeroMQ. Stanowi on wartościową platformę edukacyjną oraz bazę do dalszego rozwoju bardziej zaawansowanych i odpornych na awarie systemów rozproszonych. Zrozumienie kompromisów między prostotą implementacji, wydajnością a odpornością na błędy jest kluczowe przy projektowaniu takich systemów.
