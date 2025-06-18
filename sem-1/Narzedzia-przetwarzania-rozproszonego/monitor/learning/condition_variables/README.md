# Distributed Monitor - Etap 3: Condition Variables

## Cel Etapu
Rozszerzenie prostego distributed lock o mechanizmy condition variables - podstawę prawdziwego monitora. Implementujemy operacje wait(), signal() i broadcast() umożliwiające synchronizację procesów oczekujących na określone warunki.

## Nowe Funkcjonalności

### 1. Condition Variables
- `wait(condition)` - zwalnia mutex i blokuje proces do obudzenia
- `signal(condition)` - budzi jeden proces oczekujący na warunek  
- `broadcast(condition)` - budzi wszystkie procesy oczekujące na warunek

### 2. Semantyka Monitora
- Process musi mieć mutex żeby wykonać wait/signal/broadcast
- `wait()` zwalnia mutex i dodaje proces do kolejki warunku
- Po obudzeniu proces musi ponownie zdobyć mutex
- signal/broadcast może wykonać tylko właściciel mutex

### 3. Zarządzanie Kolejkami
- `mutex_queue` - procesy oczekujące na dostęp do sekcji krytycznej
- `condition_queues` - procesy oczekujące na konkretne warunki
- Centralna koordynacja wszystkich kolejek przez serwer

## Struktura Plików
```
step3_condition_variables/
├── monitor_server_step3.py    # Serwer z obsługą condition variables
├── monitor_client_step3.py    # Klient z API wait/signal/broadcast
├── bounded_buffer_test.py     # Test Producer-Consumer
├── test_step3_runner.py       # Automatyczne testy
└── README_step3.md           # Ta dokumentacja
```

## Architektura

### Monitor Server
```
┌─────────────────────────────────┐
│         Monitor Server          │
├─────────────────────────────────┤
│ mutex_owner: Optional[str]      │ ← Kto ma mutex
│ mutex_queue: deque              │ ← Kolejka do mutex  
│ condition_queues: Dict[str,deque]│ ← Kolejki warunków
│ clients: Dict[str,ClientState]  │ ← Stan klientów
└─────────────────────────────────┘
```

### Protokół Komunikacji
- `ENTER_MONITOR` → `GRANTED` / `QUEUED`
- `EXIT_MONITOR` → `RELEASED`
- `WAIT_CONDITION` → `WAITING`
- `SIGNAL_CONDITION` → `SIGNALED`
- `BROADCAST_CONDITION` → `BROADCASTED`

## Sekwencja Wait/Signal
```
Process A          Monitor Server          Process B
   │                     │                    │
   ├─ ENTER_MONITOR ────→│                    │
   │←──── GRANTED ───────┤                    │
   │                     │                    │
   ├─ WAIT_CONDITION ───→│ (A zwalnia mutex)  │
   │←──── WAITING ───────┤                    │
   │     (A blokuje)     │                    │
   │                     │←─ ENTER_MONITOR ───┤  
   │                     ├──── GRANTED ──────→│
   │                     │                    │
   │                     │←─ SIGNAL_CONDITION─┤
   │                     ├──── SIGNALED ─────→│
   │                     │ (B budzi A)        │
   │                     │ (A wraca do mutex_queue)
   │                     │                    │
   │                     │←─ EXIT_MONITOR ────┤
   │                     ├──── RELEASED ─────→│
   │                     │                    │
   │←──── GRANTED ───────┤ (A dostaje mutex)  │
   │     (A odblokuje)   │                    │
```

## Kluczowe Algorytmy

### Wait Algorithm
1. Sprawdź czy proces ma mutex (jeśli nie - błąd)
2. Zwolnij mutex 
3. Dodaj proces do kolejki warunku
4. Jeśli jest ktoś w mutex_queue - daj mu mutex
5. Blokuj proces (zwróć WAITING)
6. Po obudzeniu - dodaj do mutex_queue

### Signal Algorithm  
1. Sprawdź czy proces ma mutex (jeśli nie - błąd)
2. Jeśli kolejka warunku pusta - nic nie rób
3. Weź pierwszy proces z kolejki warunku
4. Dodaj go do mutex_queue
5. Zwróć SIGNALED

### Broadcast Algorithm
1. Sprawdź czy proces ma mutex (jeśli nie - błąd) 
2. Weź wszystkie procesy z kolejki warunku
3. Dodaj je wszystkie do mutex_queue
4. Zwróć BROADCASTED

## Przykład Użycia

### Producer-Consumer z Bounded Buffer
```python
# Producer
monitor = MonitorClient("tcp://localhost:5555")
monitor.enter()
try:
    while buffer_full():
        monitor.wait("not_full")
    
    add_item_to_buffer()
    monitor.signal("not_empty")
finally:
    monitor.exit()

# Consumer  
monitor = MonitorClient("tcp://localhost:5555")
monitor.enter()
try:
    while buffer_empty():
        monitor.wait("not_empty")
    
    item = remove_item_from_buffer()
    monitor.signal("not_full")
finally:
    monitor.exit()
```

## Uruchomienie

### 1. Uruchom serwer
```bash
cd step3_condition_variables
python monitor_server_step3.py
```

### 2. Uruchom test Producer-Consumer
```bash
# W drugim terminalu
python bounded_buffer_test.py
```

### 3. Uruchom automatyczne testy
```bash
python test_step3_runner.py
```

## Testy

### Test 1: Podstawowy Wait/Signal
- Proces A: enter → wait
- Proces B: enter → signal → exit  
- Proces A: otrzymuje powiadomienie → exit

### Test 2: Producer-Consumer
- Wielokrotni producenci i konsumenci
- Synchronizacja przez condition variables
- Weryfikacja poprawności bufora

### Test 3: Broadcast
- Wiele procesów czeka na warunek
- Jeden proces robi broadcast
- Wszystkie procesy zostają obudzone

## Przewidywane Problemy

### 1. Spurious Wakeups
**Problem**: Proces obudza się ale warunek dalej nie jest spełniony
**Rozwiązanie**: Zawsze używaj `wait()` w pętli while

### 2. Deadlock
**Problem**: Procesy czekają na siebie nawzajem  
**Rozwiązanie**: Uważne projektowanie protokołu, timeout

### 3. Lost Signals
**Problem**: Signal wysłany gdy nikt nie czeka
**Rozwiązanie**: To jest poprawne zachowanie w monitorach

## Metryki Wydajności
- Czas odpowiedzi wait/signal: < 1ms
- Throughput: > 1000 operacji/s
- Skalowalność: testowane z 10+ procesami

## Następne Kroki
Po ukończeniu etapu 3, przechodzimy do etapu 4:
- Pełna aplikacja BoundedBuffer
- Porównanie z lokalnym monitorem  
- Testy wydajności i stresu
- Dokumentacja końcowa