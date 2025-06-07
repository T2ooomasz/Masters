# Etap 1: Podstawy ØMQ - Instrukcja

## Instalacja wymaganych bibliotek

```bash
# Instalacja pyzmq
pip install pyzmq

# Opcjonalnie - dla lepszych logów
pip install colorlog
```

## Struktura plików

```
podstawy_zeroMQ/
├── hello_server.py    # Serwer REP
├── hello_client.py    # Klient REQ
└── README.md         # Ta instrukcja
```

## Uruchomienie

### 1. Uruchom serwer (w pierwszym terminalu)
```bash
cd podstawy_zeroMQ/
python hello_server.py
```

Powinieneś zobaczyć:
```
12:34:56 - INFO - Uruchamianie Hello Server...
12:34:56 - INFO - Utworzono kontekst ØMQ
12:34:56 - INFO - Utworzono socket REP
12:34:56 - INFO - Serwer nasłuchuje na tcp://*:5555
12:34:56 - INFO - Serwer gotowy do obsługi klientów...
12:34:56 - INFO - Oczekiwanie na wiadomość...
```

### 2. Uruchom klienta (w drugim terminalu)
```bash
cd podstawy_zeroMQ/
python hello_client.py
```

### 3. Tryb interaktywny klienta
```bash
python hello_client.py -i
```

## Co obserwować

### W terminalu serwera:
- Informacje o odbieranych wiadomościach
- Licznik obsłużonych wiadomości
- Timestampy operacji

### W terminalu klienta:
- Informacje o wysyłanych wiadomościach
- Otrzymywane odpowiedzi
- Opóźnienia między wiadomościami

## Eksperymenty do przeprowadzenia

### 1. Test podstawowy
- Uruchom serwer i klienta
- Obserwuj wymianę wiadomości
- Zatrzymaj klienta (Ctrl+C) i uruchom ponownie

### 2. Test wielokrotnych klientów
```bash
# Terminal 1: Serwer
python hello_server.py

# Terminal 2: Klient 1
python hello_client.py

# Terminal 3: Klient 2 (uruchom gdy pierwszy skończy)
python hello_client.py
```

### 3. Test trybu interaktywnego
```bash
# Terminal 1: Serwer
python hello_server.py

# Terminal 2: Klient interaktywny
python hello_client.py -i
```

Wpisuj różne wiadomości i obserwuj odpowiedzi

### 4. Test awarii serwera
- Uruchom serwer i klienta
- Zatrzymaj serwer (Ctrl+C) podczas działania klienta
- Obserwuj co się dzieje z klientem
- Uruchom serwer ponownie

## Kluczowe obserwacje

### Wzorzec REQ-REP
- **Synchroniczność**: klient czeka na odpowiedź przed wysłaniem kolejnej wiadomości
- **Kolejność**: wiadomości są obsługiwane jedna po drugiej
- **Niezawodność**: ØMQ gwarantuje doręczenie w ramach jednego połączenia

### Zarządzanie połączeniami
- **bind() vs connect()**: serwer binduje, klient łączy
- **Context**: jeden na aplikację, zarządza wszystkimi socketami
- **Cleanup**: zawsze zamykaj sockety i context

### Obsługa błędów
- **Timeouty**: domyślnie REQ-REP blokuje w nieskończoność
- **Reconnection**: ØMQ automatycznie odłącza przy błędach
- **Exception handling**: zawsze obsługuj ZMQError

## Następne kroki

Po zrozumieniu tego kodu przechodzimy do **Etapu 2**: wzorca REQ-REP z logowaniem stanu i obsługą wielu klientów równocześnie.

## Troubleshooting

### Problem: "Address already in use"
```bash
# Sprawdź co używa portu 5555
lsof -i :5555
# Lub zmień port w kodzie
```

### Problem: "Connection refused"
- Upewnij się, że serwer jest uruchomiony przed klientem
- Sprawdź czy adres `localhost:5555` jest poprawny

### Problem: Klient się zawiesza
- REQ socket musi otrzymać odpowiedź przed wysłaniem kolejnej wiadomości
- Sprawdź czy serwer wysyła odpowiedź na każde żądanie