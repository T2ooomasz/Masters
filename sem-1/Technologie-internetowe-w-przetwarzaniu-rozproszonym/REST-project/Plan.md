# Plan Projektu REST - System Zarządzania Biblioteką

## 1. Koncepcja Aplikacji

**Temat:** System zarządzania biblioteką cyfrową
**Uzasadnienie:** Temat jest intuicyjny, pozwala na naturalną hierarchię zasobów i implementację wszystkich wymaganych funkcjonalności REST.

## 2. Wybór Technologii

### Backend Framework
Python + Flask/FastAPI

### Baza Danych
**Rekomendacja: MongoDB (z Mongoose ODM)**
- NoSQL dobrze współgra z JSON
- Elastyczne schematy
- Łatwe stronicowanie

## 3. Hierarchia Zasobów

```
/api/v1/
├── /books                    # Kolekcja książek
│   ├── /{bookId}            # Pojedyncza książka
│   ├── /{bookId}/reviews    # Kolekcja recenzji książki
│   └── /{bookId}/reviews/{reviewId}  # Pojedyncza recenzja
├── /authors                 # Kolekcja autorów
│   └── /{authorId}          # Pojedynczy autor
├── /users                   # Kolekcja użytkowników
│   ├── /{userId}            # Pojedynczy użytkownik
│   └── /{userId}/loans      # Wypożyczenia użytkownika
├── /loans                   # Kolekcja wypożyczeń
│   └── /{loanId}            # Pojedyncze wypożyczenie
├── /orders                  # Zlecenia (POST once exactly)
│   └── /{orderId}           # Pojedyncze zlecenie
└── /batch-operations        # Kontroler do operacji batch
    ├── /bulk-return         # Zwrot wielu książek
    └── /bulk-loan           # Wypożyczenie wielu książek
```

## 4. Implementacja Wymagań REST

### 4.1 Proste zasoby CRUD
**Zasób: `/authors/{authorId}`**
- GET - pobierz autora
- POST - utwórz autora (na kolekcji /authors)
- PUT - aktualizuj autora
- DELETE - usuń autora

### 4.2 Zasoby-kolekcje ze stronicowaniem
**Zasób: `/books`**
```
GET /books?page=1&limit=10&sort=title&order=asc&author=Tolkien
Response:
{
  "data": [...],
  "pagination": {
    "page": 1,
    "limit": 10,
    "total": 150,
    "totalPages": 15,
    "hasNext": true,
    "hasPrev": false
  }
}
```

### 4.3 Aktualizacja warunkowa (Lost Update Problem)
**Zasób: `/books/{bookId}`**
```
PUT /books/123
Headers:
  If-Match: "etag-value-12345"
  Content-Type: application/json

Body:
{
  "title": "Updated Title",
  "availableCopies": 5
}
```
- Zwraca 412 Precondition Failed jeśli ETag się nie zgadza
- Zwraca 200 + nowy ETag przy sukcesie

### 4.4 POST once exactly
**Zasób: `/orders`**
```
POST /orders
Headers:
  Idempotency-Key: "unique-client-key-12345"
  Content-Type: application/json

Body:
{
  "userId": "user123",
  "bookIds": ["book1", "book2"],
  "type": "loan"
}
```
- Serwer zapamiętuje Idempotency-Key
- Powtórne żądanie z tym samym kluczem zwraca ten sam rezultat
- TTL na klucze: 24h

### 4.5 Zasoby-kontrolery
**Zasób: `/batch-operations/bulk-return`**
```
POST /batch-operations/bulk-return
Content-Type: application/json

{
  "loanIds": ["loan1", "loan2", "loan3"],
  "returnDate": "2025-06-10T10:00:00Z"
}
```
- Atomicznie aktualizuje wszystkie wypożyczenia
- Aktualizuje dostępność książek
- Zapisuje historię operacji

## 5. Formaty Danych

### 5.1 Książka
```json
{
  "id": "book123",
  "title": "Władca Pierścieni",
  "authors": ["author1", "author2"],
  "isbn": "978-83-7469-000-0",
  "publishYear": 1954,
  "genre": ["fantasy", "przygodowa"],
  "totalCopies": 10,
  "availableCopies": 7,
  "description": "Opis książki...",
  "createdAt": "2025-01-01T00:00:00Z",
  "updatedAt": "2025-06-10T10:00:00Z",
  "etag": "abc123def456"
}
```

### 5.2 Autor
```json
{
  "id": "author1",
  "firstName": "John",
  "lastName": "Tolkien",
  "dateOfBirth": "1892-01-03",
  "nationality": "British",
  "biography": "Opis autora...",
  "createdAt": "2025-01-01T00:00:00Z",
  "updatedAt": "2025-06-10T10:00:00Z",
  "etag": "xyz789abc123"
}
```

### 5.3 Użytkownik
```json
{
  "id": "user123",
  "email": "user@example.com",
  "firstName": "Jan",
  "lastName": "Kowalski",
  "registrationDate": "2025-01-01T00:00:00Z",
  "isActive": true,
  "maxLoans": 5,
  "currentLoans": 2,
  "etag": "user456def789"
}
```

### 5.4 Wypożyczenie
```json
{
  "id": "loan123",
  "userId": "user123",
  "bookId": "book123",
  "loanDate": "2025-06-01T10:00:00Z",
  "dueDate": "2025-06-15T23:59:59Z",
  "returnDate": null,
  "status": "active",
  "renewalCount": 0,
  "maxRenewals": 3,
  "etag": "loan789ghi012"
}
```

## 6. Kody Odpowiedzi HTTP

| Kod | Znaczenie | Kiedy używać |
|-----|-----------|--------------|
| 200 | OK | Sukces GET, PUT |
| 201 | Created | Sukces POST (tworzenie) |
| 204 | No Content | Sukces DELETE |
| 400 | Bad Request | Błędne dane wejściowe |
| 401 | Unauthorized | Brak autoryzacji |
| 403 | Forbidden | Brak uprawnień |
| 404 | Not Found | Zasób nie istnieje |
| 409 | Conflict | Konflikt biznesowy |
| 412 | Precondition Failed | Błędny ETag |
| 422 | Unprocessable Entity | Błędy walidacji |
| 429 | Too Many Requests | Rate limiting |
| 500 | Internal Server Error | Błąd serwera |

## 7. Struktura Projektu

```
library-rest-api/
├── src/
│   ├── controllers/
│   │   ├── books.py
│   │   ├── authors.py
│   │   ├── users.py
│   │   ├── loans.py
│   │   └── batch-operations.py
│   ├── models/
│   │   ├── Book.py
│   │   ├── Author.py
│   │   ├── User.py
│   │   └── Loan.py
│   ├── middleware/
│   │   ├── auth.py
│   │   ├── validation.py
│   │   ├── etag.py
│   │   └── idempotency.py
│   ├── routes/
│   │   └── api.py
│   ├── utils/
│   │   ├── pagination.py
│   │   └── errors.py
│   └── app.py
├── tests/
├── docs/
├── package.json
└── README.md
```

## 8. Przykładowe Scenariusze Testowe

### 8.1 CRUD na autorze
1. POST /authors (utworzenie)
2. GET /authors/{id} (odczyt)
3. PUT /authors/{id} (aktualizacja)
4. DELETE /authors/{id} (usunięcie)

### 8.2 Stronicowanie książek
1. GET /books?page=1&limit=5
2. GET /books?page=2&limit=5
3. Weryfikacja linków next/prev

### 8.3 Lost Update Prevention
1. GET /books/{id} (pobranie ETag)
2. PUT z poprawnym ETag (sukces)
3. PUT z nieaktualnym ETag (412 error)

### 8.4 Idempotency
1. POST /orders z Idempotency-Key
2. Powtórny POST z tym samym kluczem
3. Weryfikacja identycznej odpowiedzi

### 8.5 Batch Operations
1. POST /batch-operations/bulk-loan
2. Weryfikacja atomowości operacji
3. Test rollback przy błędzie

## 9. Plan Implementacji

### Faza 1: Setup
- Konfiguracja Python + FastAPI
- Podłączenie MongoDB
- Podstawowa struktura projektu

### Faza 2: Modele i CRUD
- Modele danych (Mongoose)
- Podstawowe operacje CRUD
- Walidacja danych

### Faza 3: Funkcjonalności REST
- Stronicowanie
- ETag i warunkowe aktualizacje
- Idempotency
- Batch operations

### Faza 4: Testy i Dokumentacja
- Testowanie w Postman
- Dokumentacja API
- Prezentacja

## 10. Prezentacja Projektu

### Narzędzia demonstracji:
1. **Postman Collection** - przygotowane zapytania
2. **Curl scripts** - przykłady z linii poleceń
3. **Simple HTML client** - podstawowy interfejs (opcjonalnie)

### Scenariusz prezentacji:
1. Demonstracja CRUD na autorach
2. Stronicowanie kolekcji książek
3. Test Lost Update Problem
4. Pokazanie POST once exactly
5. Batch operation na wypożyczeniach

Ten plan zapewnia pełną implementację wszystkich wymagań projektu przy jednoczesnym zachowaniu edukacyjnego charakteru zadania.