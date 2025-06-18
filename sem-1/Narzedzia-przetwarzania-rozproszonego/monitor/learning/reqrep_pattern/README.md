# Etap 2: REQ-REP Pattern - Szczegółowy opis

## Struktura plików
```
step2_reqrep_pattern/
├── req_client.py     # Zaawansowany klient REQ-REP
├── rep_server.py     # Serwer z routing'iem żądań
└── README.md         # Ten plik
```

## Instalacja i uruchomienie

### 1. Uruchomienie serwera
```bash
cd step2_reqrep_pattern/
python rep_server.py
```

Serwer uruchomi się na porcie 5555 i będzie logować wszystkie operacje.

### 2. Uruchomienie klienta

**Tryb automatycznej demonstracji:**
```bash
python req_client.py --demo
```

**Tryb interaktywny:**
```bash
python req_client.py --interactive
```

**Niestandardowy serwer:**
```bash
python req_client.py --server tcp://192.168.1.100:5555 --timeout 3000
```

## Kluczowe koncepcje Etapu 2

### 1. JSON Messaging Protocol

W przeciwieństwie do prostych stringów z Etapu 1, używamy strukturalnych wiadomości JSON:

**Żądanie:**
```json
{
    "type": "CALCULATE",
    "operation": "add",
    "a": 10,
    "b": 5,
    "client_id": "Client-143052-456"
}
```

**Odpowiedź:**
```json
{
    "status": "success",
    "type": "CALCULATE_RESPONSE",
    "operation": "add",
    "operands": {"a": 10, "b": 5},
    "result": 15,
    "calculation_time_ms": 50.0,
    "timestamp": "2025-01-15T14:30:52.123456"
}
```

### 2. Request Routing w Serwerze

Serwer używa wzorca **Handler Pattern** dla różnych typów żądań:

```python
self.request_handlers = {
    'PING': self._handle_ping,
    'ECHO': self._handle_echo,
    'STATUS': self._handle_status,
    'CALCULATE': self._handle_calculate,
    'TIMESTAMP': self._handle_timestamp,
    'UPTIME': self._handle_uptime
}
```

**Proces routing'u:**
1. **Parse JSON** - konwersja string → dict
2. **Extract type** - wyciągnięcie `request['type']`
3. **Find handler** - wyszukanie funkcji obsługującej
4. **Execute handler** - wywołanie z mierzeniem czasu
5. **Send response** - serializacja dict → JSON string

## Szczegółowy opis funkcji

### Server Functions (`REQREPServer`)

#### `__init__(self, port=5555)`
**Parametry:**
- `port` - port nasłuchiwania (domyślnie 5555)

**Inicjalizuje:**
- `self.context` - główny kontekst ØMQ
- `self.socket` - socket REP do komunikacji
- `self.server_stats` - dict ze statystykami serwera
- `self.request_handlers` - mapa typu żądania → funkcja obsługująca
- `self.is_running` - flaga kontrolująca główną pętlę

#### `start(self)`
**Główny proces uruchomienia:**
1. `self.socket.bind(address)` - bind socket do portu
2. `self.is_running = True` - aktywacja głównej pętli
3. `self._main_loop()` - uruchomienie pętli obsługi żądań

#### `_main_loop(self)`
**Główna pętla serwera - serce systemu:**

```python
while self.is_running:
    # 1. Odbierz żądanie
    raw_request = self.socket.recv_string()
    
    # 2. Parse JSON
    request = json.loads(raw_request)
    
    # 3. Routing
    request_type = request.get('type')
    handler = self.request_handlers.get(request_type)
    
    # 4. Wykonaj handler
    response = handler(request)
    
    # 5. Wyślij odpowiedź
    self.socket.send_string(json.dumps(response))
```

**Obsługa błędów:**
- `json.JSONDecodeError` - błędny JSON → zwraca error response
- `KeyError` - nieznany typ żądania → zwraca UNKNOWN_REQUEST_TYPE
- `Exception` w handler'ze → zwraca HANDLER_ERROR

#### Handler Functions

**`_handle_ping(self, request)`**
- **Cel:** Test łączności serwer-klient
- **Czas przetwarzania:** ~0ms (natychmiastowa odpowiedź)
- **Zwraca:** PONG z timestamp'em i client_id

**`_handle_echo(self, request)`**
- **Cel:** Odbicie wiadomości z prefix'em
- **Parametry:** `request['message']` - tekst do odbicia
- **Czas przetwarzania:** 100ms (symulacja)
- **Zwraca:** Oryginalną i odbicia wiadomość

**`_handle_calculate(self, request)`**
- **Cel:** Wykonanie obliczeń matematycznych
- **Parametry:** 
  - `operation` - typ operacji (add/subtract/multiply/divide/power)
  - `a`, `b` - operandy (liczby)
- **Różne czasy przetwarzania:**
  - add/subtract: 50ms
  - multiply: 100ms
  - divide: 150ms
  - power: 200ms
- **Obsługa błędów:** Division by zero, unknown operation

**`_handle_status(self, request)`**
- **Cel:** Zwrócenie pełnych statystyk serwera
- **Zwraca:**
  - Uptime serwera
  - Liczbę obsłużonych żądań
  - Success rate (%)
  - Średni czas odpowiedzi
  - Breakdown per request type

#### `_update_stats(self, request_type, response_time, success)`
**Kluczowa funkcja dla monitoringu:**

```python
# Podstawowe liczniki
self.server_stats['total_requests'] += 1
if success:
    self.server_stats['successful_requests'] += 1
else:
    self.server_stats['failed_requests'] += 1

# Średni czas odpowiedzi - incremental update
if success:
    total_time = self.server_stats['total_response_time'] + response_time
    total_successful = self.server_stats['successful_requests']
    self.server_stats['average_response_time'] = total_time / total_successful
```

### Client Functions (`REQREPClient`)

#### `__init__(self, server_address, timeout_ms)`
**Parametry:**
- `server_address` - adres serwera (tcp://host:port)
- `timeout_ms` - timeout w milisekundach

**Inicjalizuje:**
- Unikalny `client_id` - identyfikator na podstawie czasu i hash'u obiektu
- `request_stats` - dict z metrykami klienta
- Socket REQ z timeout'ami

#### `connect(self)`
**Proces połączenia:**
1. Ustaw timeout'y na socket: `RCVTIMEO`, `SNDTIMEO`
2. Połącz z serwerem: `socket.connect(address)`
3. Test połączenia: wyślij PING i sprawdź PONG
4. Zwróć `True` jeśli test udany, `False` w przeciwnym razie

#### `_send_request(self, request)`
**Uniwersalna funkcja wysyłania żądań:**

```python
def _send_request(self, request):
    # 1. Dodaj client_id
    request['client_id'] = self.client_id
    
    # 2. Serializuj do JSON
    request_json = json.dumps(request)
    
    # 3. Wyślij żądanie
    self.socket.send_string(request_json)
    
    # 4. Odbierz odpowiedź (z timeout'em)
    response_json = self.socket.recv_string()  # Może rzucić zmq.Again
    
    # 5. Deserializuj odpowiedź
    response = json.loads(response_json)
    
    return response
```

**Obsługa błędów:**
- `zmq.Again` - timeout → zwraca None, aktualizuje statystyki timeout'ów
- `json.JSONDecodeError` - błędna odpowiedź → zwraca None
- Inne wyjątki → loguje błąd, zwraca None

#### Specific Request Methods

**`ping(self)`, `echo(message)`, `get_status()`, etc.**
- Każda metoda tworzy odpowiedni dict z żądaniem
- Deleguje do `_send_request()`
- Zwraca odpowiedź lub None

#### `run_interactive_mode(self)`
**Tryb interaktywny - REPL (Read-Eval-Print Loop):**

```
[Client-143052-456]> ping
PONG: Server is alive

[Client-143052-456]> calc add 10 5
add(10.0, 5.0) = 15.0

[Client-143052-456]> echo Hello World
Echo: ECHO: Hello World

[Client-143052-456]> stats
==================================================
CLIENT STATISTICS - Client-143052-456
==================================================
Total requests: 3
Successful: 3
Failed: 0
Success rate: 100.0%
Average response time: 0.125s
```

**Dostępne komendy:**
- `ping` - test połączenia
- `echo <message>` - odbicie wiadomości
- `calc <op> <a> <b>` - kalkulacje
- `status` - statystyki serwera
- `time` - czas serwera
- `uptime` - uptime serwera
- `stats` - statystyki klienta
- `quit` - wyjście

#### `run_automatic_demo(self)`
**Zautomatyzowana demonstracja:**
- Wykonuje predefiniowaną sekwencję żądań
- Wyświetla wyniki każdego żądania
- Na końcu pokazuje statystyki klienta

## Kluczowe zmienne i ich znaczenie

### Server Variables

#### `self.server_stats`
```python
{
    'start_time': datetime.now(),           # Kiedy serwer został uruchomiony
    'total_requests': 0,                    # Wszystkie żądania
    'successful_requests': 0,               # Udane żądania
    'failed_requests': 0,                   # Nieudane żądania
    'request_types': {                      # Breakdown per typ
        'PING': 5,
        'ECHO': 3,
        'CALCULATE': 7
    },
    'average_response_time': 0.0,           # Średni czas w sekundach
    'total_response_time': 0.0              # Suma czasów (dla średniej)
}
```

#### `self.request_handlers`
```python
{
    'PING': self._handle_ping,              # Funkcja obsługująca PING
    'ECHO': self._handle_echo,              # Funkcja obsługująca ECHO
    'CALCULATE': self._handle_calculate,    # etc.
    # ...
}
```

### Client Variables

#### `self.client_id`
- Format: `"Client-HHMMSS-XXX"`
- `HHMMSS` - czas utworzenia klienta
- `XXX` - ostatnie 3 cyfry hash'u obiektu
- Przykład: `"Client-143052-456"`

#### `self.request_stats`
```python
{
    'total_requests': 0,                    # Wszystkie wysłane żądania
    'successful_requests': 0,               # Udane (otrzymano odpowiedź)
    'failed_requests': 0,                   # Nieudane (błąd/timeout)
    'timeouts': 0,                          # Subset of failed - timeout'y
    'total_response_time': 0.0,             # Suma czasów odpowiedzi
    'min_response_time': float('inf'),      # Najszybsza odpowiedź
    'max_response_time': 0.0,               # Najwolniejsza odpowiedź
    'request_types': {                      # Per typ żądania
        'PING': {
            'total': 3, 
            'success': 3, 
            'failed': 0
        }
    }
}
```

## Wzorce komunikacji w Etapie 2

### 1. Synchroniczna komunikacja
```
Klient                    Serwer
  |                         |
  |----JSON Request-------->|  1. send_string(json)
  |                         |  2. recv_string() 
  |                         |  3. JSON parse
  |                         |  4. Route to handler
  |                         |  5. Execute handler
  |                         |  6. JSON serialize
  |<---JSON Response--------|  7. send_string(json)
  |                         |
  8. recv_string()          |
  9. JSON parse             |
```

### 2. Timeout handling
```python
# Ustawienie timeout'ów
socket.setsockopt(zmq.RCVTIMEO, 5000)  # 5 sekund receive timeout
socket.setsockopt(zmq.SNDTIMEO, 5000)  # 5 sekund send timeout

try:
    response = socket.recv_string()
except zmq.Again:
    # Timeout - serwer nie odpowiedział w czasie
    print("Request timeout")
```

### 3. Error propagation
```python
# Serwer
try:
    result = risky_operation()
    return {'status': 'success', 'result': result}
except Exception as e:
    return {'status': 'error', 'error': str(e)}

# Klient
response = send_request(req)
if response and response.get('status') == 'success':
    # Sukces
    process_result(response['result'])
else:
    # Błąd lub timeout
    handle_error(response)
```

## Różnice względem Etapu 1

| Aspekt | Etap 1 | Etap 2 |
|--------|--------|--------|
| **Protokół** | Plain text strings | Structured JSON |
| **Routing** | Brak - jedna funkcja | Handler pattern |
| **Error handling** | Podstawowy | Structured error responses |
| **Timeouts** | Brak | Configurable timeouts |
| **Statistics** | Podstawowe logowanie | Detailed metrics |
| **Client modes** | Tylko automatyczny | Interactive + Demo |
| **Request types** | Jeden typ (hello) | 6 różnych typów |

## Przygotowanie do Etapu 3

Etap 2 wprowadza kluczowe koncepcje potrzebne dla distributed lock:

1. **JSON messaging** - strukturalna komunikacja
2. **Request routing** - różne typy operacji  
3. **Error handling** - obsługa błędów systemu rozproszonego
4. **Timeouts** - ochrona przed hanging'iem
5. **Client identification** - unikalny ID per klient
6. **Statistics tracking** - monitoring systemu

W Etapie 3 wykorzystamy te mechanizmy do zbudowania prostego distributed mutex z operacjami `ACQUIRE_LOCK` i `RELEASE_LOCK`.

## Testowanie

### Test podstawowy
1. Uruchom serwer w jednym terminalu
2. Uruchom klienta w trybie demo w drugim terminalu
3. Obserwuj logi w obu terminalach

### Test timeout'ów
1. Uruchom klienta z krótkim timeout'em: `--timeout 100`
2. Zatrzymaj serwer (Ctrl+C)
3. Spróbuj wykonać żądanie - powinien wystąpić timeout

### Test wielu klientów
1. Uruchom serwer
2. Uruchom 3 klientów jednocześnie w trybie demo
3. Sprawdź statystyki serwera - powinien obsłużyć wszystkie żądania

Ten etap stanowi solidną bazę dla implementacji distributed monitor w kolejnych krokach!