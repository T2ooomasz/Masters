# Projekt Usługi REST - System Zarządzania Biblioteką

## 1. Hierarchia Zasobów

### 1.1 Struktura Hierarchiczna

```
/api/v1/
├── /authors                              # Kolekcja autorów
│   └── /{authorId}                       # Pojedynczy autor
├── /books                                # Kolekcja książek
│   ├── /{bookId}                         # Pojedyncza książka
│   └── /{bookId}/reviews                 # Kolekcja recenzji książki
│       └── /{reviewId}                   # Pojedyncza recenzja
├── /users                                # Kolekcja użytkowników
│   ├── /{userId}                         # Pojedynczy użytkownik
│   └── /{userId}/loans                   # Kolekcja wypożyczeń użytkownika
├── /loans                                # Kolekcja wszystkich wypożyczeń
│   └── /{loanId}                         # Pojedyncze wypożyczenie
├── /orders                               # Kolekcja zleceń (POST once exactly)
│   └── /{orderId}                        # Pojedyncze zlecenie
└── /batch-operations                     # Kontrolery batch
    ├── /bulk-loan                        # Kontroler masowego wypożyczenia
    ├── /bulk-return                      # Kontroler masowego zwrotu
    └── /inventory-update                 # Kontroler aktualizacji inwentarza
```

### 1.2 Relacje między Zasobami

| Zasób Nadrzędny | Zasób Podrzędny | Typ Relacji | Opis |
|-----------------|-----------------|-------------|------|
| /books | /books/{id}/reviews | 1:N | Książka może mieć wiele recenzji |
| /users | /users/{id}/loans | 1:N | Użytkownik może mieć wiele wypożyczeń |
| /authors | /books | N:M | Autor może napisać wiele książek, książka może mieć wielu autorów |
| /users | /loans | 1:N | Użytkownik może mieć wiele wypożyczeń |
| /books | /loans | 1:N | Książka może być wypożyczona wiele razy |

## 2. Operacje HTTP na Zasobach

### 2.1 Kolekcja Autorów (/authors)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /authors | Pobierz listę autorów | 200 | Zwraca stronicowaną listę autorów z możliwością filtrowania |
| POST | /authors | Utwórz nowego autora | 201, 400, 409, 422 | Tworzy nowego autora w systemie |
| HEAD | /authors | Metadane kolekcji | 200 | Zwraca nagłówki bez body (liczba autorów, ETag kolekcji) |

### 2.2 Pojedynczy Autor (/authors/{authorId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /authors/{id} | Pobierz dane autora | 200, 404 | Zwraca szczegóły konkretnego autora |
| PUT | /authors/{id} | Aktualizuj autora | 200, 400, 404, 412, 422 | Pełna aktualizacja danych autora z obsługą ETag |
| PATCH | /authors/{id} | Częściowa aktualizacja | 200, 400, 404, 412, 422 | Częściowa aktualizacja wybranych pól |
| DELETE | /authors/{id} | Usuń autora | 204, 404, 409 | Usuwa autora (jeśli nie ma powiązanych książek) |
| HEAD | /authors/{id} | Metadane autora | 200, 404 | Zwraca nagłówki z ETag do walidacji |

### 2.3 Kolekcja Książek (/books)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /books | Pobierz listę książek | 200 | Stronicowana lista z filtrowaniem i sortowaniem |
| POST | /books | Utwórz nową książkę | 201, 400, 409, 422 | Dodaje nową książkę do katalogu |
| HEAD | /books | Metadane kolekcji | 200 | Informacje o liczbie książek i metadanych kolekcji |

### 2.4 Pojedyncza Książka (/books/{bookId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /books/{id} | Pobierz dane książki | 200, 404 | Szczegóły książki z dostępnością |
| PUT | /books/{id} | Aktualizuj książkę | 200, 400, 404, 412, 422 | Pełna aktualizacja z walidacją ETag (Lost Update Prevention) |
| PATCH | /books/{id} | Częściowa aktualizacja | 200, 400, 404, 412, 422 | Aktualizacja wybranych pól (np. dostępności) |
| DELETE | /books/{id} | Usuń książkę | 204, 404, 409 | Usuwa książkę (jeśli nie jest wypożyczona) |
| HEAD | /books/{id} | Metadane książki | 200, 404 | ETag dla walidacji współbieżności |

### 2.5 Kolekcja Recenzji (/books/{bookId}/reviews)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /books/{id}/reviews | Pobierz recenzje książki | 200, 404 | Lista recenzji dla konkretnej książki |
| POST | /books/{id}/reviews | Dodaj recenzję | 201, 400, 404, 409, 422 | Nowa recenzja (jeden użytkownik - jedna recenzja) |

### 2.6 Pojedyncza Recenzja (/books/{bookId}/reviews/{reviewId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /books/{bookId}/reviews/{reviewId} | Pobierz recenzję | 200, 404 | Szczegóły konkretnej recenzji |
| PUT | /books/{bookId}/reviews/{reviewId} | Aktualizuj recenzję | 200, 400, 403, 404, 412 | Autor może aktualizować swoją recenzję |
| DELETE | /books/{bookId}/reviews/{reviewId} | Usuń recenzję | 204, 403, 404 | Autor może usunąć swoją recenzję |

### 2.7 Kolekcja Użytkowników (/users)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /users | Pobierz listę użytkowników | 200 | Stronicowana lista użytkowników (admin) |
| POST | /users | Zarejestruj użytkownika | 201, 400, 409, 422 | Rejestracja nowego użytkownika |

### 2.8 Pojedynczy Użytkownik (/users/{userId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /users/{id} | Pobierz dane użytkownika | 200, 403, 404 | Profil użytkownika (własny lub admin) |
| PUT | /users/{id} | Aktualizuj użytkownika | 200, 400, 403, 404, 412 | Aktualizacja profilu z ETag |
| PATCH | /users/{id} | Częściowa aktualizacja | 200, 400, 403, 404, 412 | Zmiana wybranych danych profilu |
| DELETE | /users/{id} | Usuń konto | 204, 403, 404, 409 | Deaktywacja konta (jeśli brak aktywnych wypożyczeń) |

### 2.9 Wypożyczenia Użytkownika (/users/{userId}/loans)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /users/{id}/loans | Pobierz wypożyczenia użytkownika | 200, 403, 404 | Historia wypożyczeń konkretnego użytkownika |

### 2.10 Kolekcja Wypożyczeń (/loans)

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /loans | Pobierz wszystkie wypożyczenia | 200 | Stronicowana lista wypożyczeń (admin) |
| POST | /loans | Utwórz wypożyczenie | 201, 400, 409, 422 | Nowe wypożyczenie książki |

### 2.11 Pojedyncze Wypożyczenie (/loans/{loanId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /loans/{id} | Pobierz dane wypożyczenia | 200, 403, 404 | Szczegóły wypożyczenia |
| PUT | /loans/{id} | Aktualizuj wypożyczenie | 200, 400, 403, 404, 412 | Przedłużenie lub zwrot z ETag |
| PATCH | /loans/{id} | Częściowa aktualizacja | 200, 400, 403, 404, 412 | Zmiana statusu lub daty zwrotu |
| DELETE | /loans/{id} | Anuluj wypożyczenie | 204, 403, 404, 409 | Anulowanie (tylko jeśli nie rozpoczęte) |

### 2.12 Kolekcja Zleceń (/orders) - POST Once Exactly

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /orders | Pobierz zlecenia | 200 | Lista zleceń z możliwością filtrowania |
| POST | /orders | Utwórz zlecenie | 201, 400, 409, 422 | Nowe zlecenie z Idempotency-Key |

### 2.13 Pojedyncze Zlecenie (/orders/{orderId})

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| GET | /orders/{id} | Pobierz status zlecenia | 200, 404 | Status przetwarzania zlecenia |
| DELETE | /orders/{id} | Anuluj zlecenie | 204, 404, 409 | Anulowanie (tylko jeśli w trakcie) |

### 2.14 Kontrolery Batch Operations

| Metoda HTTP | Endpoint | Znaczenie | Kod Odpowiedzi | Opis |
|-------------|----------|-----------|----------------|------|
| POST | /batch-operations/bulk-loan | Masowe wypożyczenie | 200, 400, 422 | Atomowe wypożyczenie wielu książek |
| POST | /batch-operations/bulk-return | Masowy zwrot | 200, 400, 422 | Atomowy zwrot wielu książek |
| POST | /batch-operations/inventory-update | Aktualizacja inwentarza | 200, 400, 422 | Atomowa aktualizacja dostępności |

## 3. Formaty Danych

### 3.1 Autor (Author)

#### Wejście (POST/PUT):
```json
{
  "firstName": "John Ronald Reuel",
  "lastName": "Tolkien",
  "dateOfBirth": "1892-01-03",
  "dateOfDeath": "1973-09-02",
  "nationality": "British",
  "biography": "Brytyjski filolog, językoznawca i pisarz...",
  "website": "https://www.tolkiensociety.org"
}
```

#### Wyjście (GET):
```json
{
  "id": "author_507f1f77bcf86cd799439011",
  "firstName": "John Ronald Reuel",
  "lastName": "Tolkien",
  "fullName": "John Ronald Reuel Tolkien",
  "dateOfBirth": "1892-01-03",
  "dateOfDeath": "1973-09-02",
  "nationality": "British",
  "biography": "Brytyjski filolog, językoznawca i pisarz...",
  "website": "https://www.tolkiensociety.org",
  "booksCount": 15,
  "createdAt": "2025-01-01T10:00:00.000Z",
  "updatedAt": "2025-06-10T15:30:00.000Z",
  "etag": "\"507f1f77bcf86cd799439011-1736515800000\"",
  "_links": {
    "self": "/api/v1/authors/507f1f77bcf86cd799439011",
    "books": "/api/v1/books?author=507f1f77bcf86cd799439011"
  }
}
```

#### Częściowa aktualizacja (PATCH):
```json
{
  "biography": "Zaktualizowana biografia autora...",
  "website": "https://new-website.com"
}
```

### 3.2 Książka (Book)

#### Wejście (POST/PUT):
```json
{
  "title": "Władca Pierścieni: Drużyna Pierścienia",
  "originalTitle": "The Lord of the Rings: The Fellowship of the Ring",
  "authors": ["507f1f77bcf86cd799439011"],
  "isbn": "978-83-7469-123-4",
  "publishYear": 1954,
  "publisher": "Wydawnictwo Amber",
  "pages": 635,
  "language": "pl",
  "genres": ["fantasy", "adventure", "classic"],
  "description": "Pierwsza część trylogii Władca Pierścieni...",
  "totalCopies": 10,
  "availableCopies": 7,
  "price": 45.99,
  "coverImage": "https://example.com/covers/lotr1.jpg"
}
```

#### Wyjście (GET):
```json
{
  "id": "book_507f1f77bcf86cd799439012",
  "title": "Władca Pierścieni: Drużyna Pierścienia",
  "originalTitle": "The Lord of the Rings: The Fellowship of the Ring",
  "authors": [
    {
      "id": "507f1f77bcf86cd799439011",
      "fullName": "John Ronald Reuel Tolkien"
    }
  ],
  "isbn": "978-83-7469-123-4",
  "publishYear": 1954,
  "publisher": "Wydawnictwo Amber",
  "pages": 635,
  "language": "pl",
  "genres": ["fantasy", "adventure", "classic"],
  "description": "Pierwsza część trylogii Władca Pierścieni...",
  "totalCopies": 10,
  "availableCopies": 7,
  "price": 45.99,
  "currency": "PLN",
  "coverImage": "https://example.com/covers/lotr1.jpg",
  "averageRating": 4.8,
  "reviewsCount": 156,
  "status": "available",
  "createdAt": "2025-01-01T10:00:00.000Z",
  "updatedAt": "2025-06-10T15:30:00.000Z",
  "etag": "\"507f1f77bcf86cd799439012-1736515800000\"",
  "_links": {
    "self": "/api/v1/books/507f1f77bcf86cd799439012",
    "reviews": "/api/v1/books/507f1f77bcf86cd799439012/reviews",
    "authors": [
      "/api/v1/authors/507f1f77bcf86cd799439011"
    ]
  }
}
```

### 3.3 Użytkownik (User)

#### Wejście (POST):
```json
{
  "email": "jan.kowalski@example.com",
  "password": "SecurePassword123!",
  "firstName": "Jan",
  "lastName": "Kowalski",
  "dateOfBirth": "1990-05-15",
  "phone": "+48123456789",
  "address": {
    "street": "ul. Przykładowa 123",
    "city": "Warszawa",
    "postalCode": "00-001",
    "country": "Poland"
  }
}
```

#### Wyjście (GET):
```json
{
  "id": "user_507f1f77bcf86cd799439013",
  "email": "jan.kowalski@example.com",
  "firstName": "Jan",
  "lastName": "Kowalski",
  "fullName": "Jan Kowalski",
  "dateOfBirth": "1990-05-15",
  "phone": "+48123456789",
  "address": {
    "street": "ul. Przykładowa 123",
    "city": "Warszawa",
    "postalCode": "00-001",
    "country": "Poland"
  },
  "registrationDate": "2025-01-01T10:00:00.000Z",
  "lastLoginDate": "2025-06-10T08:30:00.000Z",
  "isActive": true,
  "role": "user",
  "maxLoans": 5,
  "currentLoansCount": 2,
  "totalLoansCount": 47,
  "penaltyPoints": 0,
  "createdAt": "2025-01-01T10:00:00.000Z",
  "updatedAt": "2025-06-10T15:30:00.000Z",
  "etag": "\"507f1f77bcf86cd799439013-1736515800000\"",
  "_links": {
    "self": "/api/v1/users/507f1f77bcf86cd799439013",
    "loans": "/api/v1/users/507f1f77bcf86cd799439013/loans"
  }
}
```

### 3.4 Wypożyczenie (Loan)

#### Wejście (POST):
```json
{
  "userId": "507f1f77bcf86cd799439013",
  "bookId": "507f1f77bcf86cd799439012",
  "loanDuration": 14
}
```

#### Wyjście (GET):
```json
{
  "id": "loan_507f1f77bcf86cd799439014",
  "userId": "507f1f77bcf86cd799439013",
  "bookId": "507f1f77bcf86cd799439012",
  "user": {
    "id": "507f1f77bcf86cd799439013",
    "fullName": "Jan Kowalski",
    "email": "jan.kowalski@example.com"
  },
  "book": {
    "id": "507f1f77bcf86cd799439012",
    "title": "Władca Pierścieni: Drużyna Pierścienia",
    "authors": ["John Ronald Reuel Tolkien"],
    "isbn": "978-83-7469-123-4"
  },
  "loanDate": "2025-06-01T10:00:00.000Z",
  "dueDate": "2025-06-15T23:59:59.999Z",
  "returnDate": null,
  "status": "active",
  "renewalCount": 0,
  "maxRenewals": 3,
  "penaltyAmount": 0.0,
  "currency": "PLN",
  "notes": "",
  "createdAt": "2025-06-01T10:00:00.000Z",
  "updatedAt": "2025-06-10T15:30:00.000Z",
  "etag": "\"507f1f77bcf86cd799439014-1736515800000\"",
  "_links": {
    "self": "/api/v1/loans/507f1f77bcf86cd799439014",
    "user": "/api/v1/users/507f1f77bcf86cd799439013",
    "book": "/api/v1/books/507f1f77bcf86cd799439012"
  }
}
```

### 3.5 Recenzja (Review)

#### Wejście (POST):
```json
{
  "userId": "507f1f77bcf86cd799439013",
  "rating": 5,
  "title": "Wspaniała książka!",
  "content": "Jedna z najlepszych książek fantasy jakie czytałem...",
  "isRecommended": true
}
```

#### Wyjście (GET):
```json
{
  "id": "review_507f1f77bcf86cd799439015",
  "bookId": "507f1f77bcf86cd799439012",
  "userId": "507f1f77bcf86cd799439013",
  "user": {
    "id": "507f1f77bcf86cd799439013",
    "fullName": "Jan Kowalski"
  },
  "rating": 5,
  "title": "Wspaniała książka!",
  "content": "Jedna z najlepszych książek fantasy jakie czytałem...",
  "isRecommended": true,
  "helpfulVotes": 12,
  "totalVotes": 15,
  "createdAt": "2025-06-05T14:20:00.000Z",
  "updatedAt": "2025-06-10T15:30:00.000Z",
  "etag": "\"507f1f77bcf86cd799439015-1736515800000\"",
  "_links": {
    "self": "/api/v1/books/507f1f77bcf86cd799439012/reviews/507f1f77bcf86cd799439015",
    "book": "/api/v1/books/507f1f77bcf86cd799439012",
    "user": "/api/v1/users/507f1f77bcf86cd799439013"
  }
}
```

### 3.6 Zlecenie (Order) - POST Once Exactly

#### Wejście (POST):
```json
{
  "type": "bulk_loan",
  "userId": "507f1f77bcf86cd799439013",
  "bookIds": [
    "507f1f77bcf86cd799439012",
    "507f1f77bcf86cd799439016",
    "507f1f77bcf86cd799439017"
  ],
  "loanDuration": 14,
  "notes": "Wypożyczenie na wakacje"
}
```

#### Wyjście (GET):
```json
{
  "id": "order_507f1f77bcf86cd799439018",
  "type": "bulk_loan",
  "userId": "507f1f77bcf86cd799439013",
  "bookIds": [
    "507f1f77bcf86cd799439012",
    "507f1f77bcf86cd799439016",
    "507f1f77bcf86cd799439017"
  ],
  "loanDuration": 14,
  "notes": "Wypożyczenie na wakacje",
  "status": "completed",
  "result": {
    "successful": [
      "507f1f77bcf86cd799439012",
      "507f1f77bcf86cd799439016"
    ],
    "failed": [
      {
        "bookId": "507f1f77bcf86cd799439017",
        "reason": "Book not available"
      }
    ],
    "createdLoans": [
      "loan_507f1f77bcf86cd799439019",
      "loan_507f1f77bcf86cd799439020"
    ]
  },
  "idempotencyKey": "user13-bulk-loan-20250610-001",
  "processedAt": "2025-06-10T16:00:00.000Z",
  "createdAt": "2025-06-10T16:00:00.000Z",
  "_links": {
    "self": "/api/v1/orders/507f1f77bcf86cd799439018",
    "user": "/api/v1/users/507f1f77bcf86cd799439013",
    "loans": [
      "/api/v1/loans/507f1f77bcf86cd799439019",
      "/api/v1/loans/507f1f77bcf86cd799439020"
    ]
  }
}
```

### 3.7 Kolekcje ze Stronicowaniem

#### Przykład: GET /books

#### Parametry zapytania:
```
GET /api/v1/books?page=2&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true
```

#### Odpowiedź:
```json
{
  "data": [
    {
      "id": "book_507f1f77bcf86cd799439012",
      "title": "Władca Pierścieni: Drużyna Pierścienia",
      "authors": [{"id": "507f1f77bcf86cd799439011", "fullName": "J.R.R. Tolkien"}],
      "availableCopies": 7,
      "totalCopies": 10,
      "averageRating": 4.8
    }
  ],
  "pagination": {
    "page": 2,
    "limit": 10,
    "total": 156,
    "totalPages": 16,
    "hasNext": true,
    "hasPrev": true,
    "nextPage": 3,
    "prevPage": 1
  },
  "filters": {
    "author": "Tolkien",
    "genre": "fantasy",
    "available": true
  },
  "sorting": {
    "field": "title",
    "order": "asc"
  },
  "_links": {
    "self": "/api/v1/books?page=2&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true",
    "first": "/api/v1/books?page=1&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true",
    "prev": "/api/v1/books?page=1&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true",
    "next": "/api/v1/books?page=3&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true",
    "last": "/api/v1/books?page=16&limit=10&sort=title&order=asc&author=Tolkien&genre=fantasy&available=true"
  }
}
```

### 3.8 Batch Operations

#### Masowe Wypożyczenie (POST /batch-operations/bulk-loan):
```json
{
  "operations": [
    {
      "userId": "507f1f77bcf86cd799439013",
      "bookId": "507f1f77bcf86cd799439012",
      "loanDuration": 14
    },
    {
      "userId": "507f1f77bcf86cd799439013",
      "bookId": "507f1f77bcf86cd799439016",
      "loanDuration": 14
    }
  ],
  "executionMode": "atomic"
}
```

#### Odpowiedź:
```json
{
  "batchId": "batch_507f1f77bcf86cd799439021",
  "status": "completed",
  "executionMode": "atomic",
  "totalOperations": 2,
  "successfulOperations": 2,
  "failedOperations": 0,
  "results": [
    {
      "operation": 1,
      "status": "success",
      "loanId": "loan_507f1f77bcf86cd799439022",
      "bookId": "507f1f77bcf86cd799439012"
    },
    {
      "operation": 2,
      "status": "success",
      "loanId": "loan_507f1f77bcf86cd799439023",
      "bookId": "507f1f77bcf86cd799439016"
    }
  ],
  "executedAt": "2025-06-10T16:15:00.000Z",
  "duration": "0.325s"
}
```

### 3.9 Formaty Błędów

#### Błąd Walidacji (422):
```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Validation failed",
    "details": [
      {
        "field": "email",
        "message": "Invalid email format"
      },
      {
        "field": "firstName",
        "message": "First name is required"
      }
    ]
  },
  "timestamp": "2025-06-10T16:20:00.000Z",
  "path": "/api/v1/books/507f1f77bcf86cd799439012",
  "method": "PUT"
}
```

#### Błąd Konfliktu Biznesowego (409):
```json
{
  "error": {
    "code": "BUSINESS_CONFLICT",
    "message": "Cannot delete author with existing books",
    "details": {
      "authorId": "507f1f77bcf86cd799439011",
      "booksCount": 15,
      "suggestion": "Remove author from all books before deletion"
    }
  },
  "timestamp": "2025-06-10T16:20:00.000Z",
  "path": "/api/v1/authors/507f1f77bcf86cd799439011",
  "method": "DELETE"
}
```

#### Błąd Not Found (404):
```json
{
  "error": {
    "code": "RESOURCE_NOT_FOUND",
    "message": "Book with id '507f1f77bcf86cd799439999' not found"
  },
  "timestamp": "2025-06-10T16:20:00.000Z",
  "path": "/api/v1/books/507f1f77bcf86cd799439999",
  "method": "GET"
}
```

## 4. Nagłówki HTTP

### 4.1 Nagłówki Żądań

| Nagłówek | Zastosowanie | Przykład | Opis |
|----------|--------------|----------|------|
| `Content-Type` | POST, PUT, PATCH | `application/json` | Format danych w body |
| `Accept` | Wszystkie | `application/json` | Preferowany format odpowiedzi |
| `If-Match` | PUT, PATCH, DELETE | `"507f1f77bcf86cd799439012-1736515800000"` | ETag dla walidacji konkurencji |
| `If-None-Match` | GET | `"507f1f77bcf86cd799439012-1736515800000"` | Walidacja cache |
| `Idempotency-Key` | POST (orders) | `user13-order-20250610-001` | Klucz idempotencji |
| `Authorization` | Wszystkie chronione | `Bearer eyJhbGciOiJIUzI1NiIs...` | Token autoryzacji |
| `X-Request-ID` | Wszystkie | `req_507f1f77bcf86cd799439030` | Identyfikator żądania |

### 4.2 Nagłówki Odpowiedzi

| Nagłówek | Zastosowanie | Przykład | Opis |
|----------|--------------|----------|------|
| `Content-Type` | Wszystkie z body | `application/json; charset=utf-8` | Format danych odpowiedzi |
| `ETag` | GET, PUT, PATCH | `"507f1f77bcf86cd799439012-1736515800000"` | Tag dla walidacji konkurencji |
| `Last-Modified` | GET | `Wed, 10 Jun 2025 15:30:00 GMT` | Data ostatniej modyfikacji |
| `Location` | 201, 202 | `/api/v1/books/507f1f77bcf86cd799439012` | URL utworzonego zasobu |
| `X-Total-Count` | Kolekcje | `156` | Łączna liczba elementów |
| `X-Page-Count` | Kolekcje | `16` | Liczba stron |
| `X-Rate-Limit-Remaining` | Wszystkie | `97` | Pozostałe żądania |
| `X-Request-ID` | Wszystkie | `req_507f1f77bcf86cd799439030` | Identyfikator żądania |
| `Cache-Control` | GET | `max-age=3600, must-revalidate` | Polityka cache |

## 5. Walidacja i Ograniczenia

### 5.1 Walidacja Danych Wejściowych

| Pole | Typ | Ograniczenia | Walidacja |
|------|-----|--------------|-----------|
| `firstName` | string | 1-50 znaków | Tylko litery, spacje, łączniki |
| `lastName` | string | 1-50 znaków | Tylko litery, spacje, łączniki |
| `email` | string | 5-100 znaków | Format email RFC 5322 |
| `isbn` | string | 10 lub 13 znaków | Format ISBN-10/13 z checksumą |
| `publishYear` | integer | 1000-2025 | Rok publikacji |
| `rating` | integer | 1-5 | Ocena recenzji |
| `totalCopies` | integer | 1-1000 | Liczba egzemplarzy |
| `loanDuration` | integer | 1-90 dni | Czas wypożyczenia |
| `phone` | string | 9-15 znaków | Format międzynarodowy |

### 5.2 Ograniczenia Biznesowe

| Zasada | Opis | Implementacja |
|--------|------|---------------|
| Maksymalne wypożyczenia | Użytkownik może mieć max 5 aktywnych wypożyczeń | Walidacja przy POST /loans |
| Unikalne ISBN | Każda książka ma unikalny ISBN | Indeks unique w bazie |
| Jeden autor - jedna recenzja | Użytkownik może dodać tylko jedną recenzję na książkę | Composite unique key |
| Dostępność książek | availableCopies ≤ totalCopies | Walidacja przy aktualizacji |
| Aktywne wypożyczenia | Nie można usunąć książki z aktywnymi wypożyczeniami | Check przed DELETE |

## 6. Statusy i Stany Zasobów

### 6.1 Stany Książki
- `available` - dostępna do wypożyczenia
- `unavailable` - wszystkie egzemplarze wypożyczone
- `maintenance` - w trakcie konserwacji
- `discontinued` - wycofana z obiegu

### 6.2 Stany Wypożyczenia
- `active` - aktywne wypożyczenie
- `overdue` - przeterminowane
- `returned` - zwrócone
- `cancelled` - anulowane
- `lost` - zgubione

### 6.3 Stany Zlecenia
- `pending` - oczekuje na przetworzenie
- `processing` - w trakcie przetwarzania
- `completed` - zakończone pomyślnie
- `failed` - zakończone niepowodzeniem
- `cancelled` - anulowane

### 6.4 Stany Użytkownika
- `active` - aktywny
- `inactive` - nieaktywny
- `suspended` - zawieszony
- `banned` - zbanowany

## 7. Przykłady Kompletnych Scenariuszy

### 7.1 Scenariusz: Wypożyczenie Książki z Walidacją ETag

#### Krok 1: Sprawdzenie dostępności
```http
GET /api/v1/books/507f1f77bcf86cd799439012
Accept: application/json
```

#### Odpowiedź:
```http
HTTP/1.1 200 OK
Content-Type: application/json
ETag: "507f1f77bcf86cd799439012-1736515800000"
{
  "id": "507f1f77bcf86cd799439012",
  "title": "Władca Pierścieni: Drużyna Pierścienia",
  "availableCopies": 3,
  "totalCopies": 10
}
```

#### Krok 2: Aktualizacja dostępności (symulacja konkurencji)
```http
PUT /api/v1/books/507f1f77bcf86cd799439012
Content-Type: application/json
If-Match: "507f1f77bcf86cd799439012-1736515800000"
{
  "availableCopies": 2
}
```

#### Odpowiedź (sukces):
```http
HTTP/1.1 200 OK
Content-Type: application/json
ETag: "507f1f77bcf86cd799439012-1736515860000"
{
  "id": "507f1f77bcf86cd799439012",
  "availableCopies": 2,
  "updatedAt": "2025-06-10T15:31:00.000Z"
}
```

#### Krok 3: Próba aktualizacji z nieaktualnym ETag
```http
PUT /api/v1/books/507f1f77bcf86cd799439012
Content-Type: application/json
If-Match: "507f1f77bcf86cd799439012-1736515800000"
{
  "availableCopies": 1
}
```

#### Odpowiedź (błąd konkurencji):
```http
HTTP/1.1 412 Precondition Failed
Content-Type: application/json
{
  "error": {
    "code": "PRECONDITION_FAILED",
    "message": "Resource has been modified by another request",
    "details": {
      "currentEtag": "507f1f77bcf86cd799439012-1736515860000",
      "providedEtag": "507f1f77bcf86cd799439012-1736515800000"
    }
  }
}
```

### 7.2 Scenariusz: POST Once Exactly dla Zlecenia

#### Krok 1: Pierwsze zlecenie
```http
POST /api/v1/orders
Content-Type: application/json
Idempotency-Key: user13-bulk-loan-20250610-001
{
  "type": "bulk_loan",
  "userId": "507f1f77bcf86cd799439013",
  "bookIds": ["507f1f77bcf86cd799439012", "507f1f77bcf86cd799439016"]
}
```

#### Odpowiedź:
```http
HTTP/1.1 201 Created
Content-Type: application/json
Location: /api/v1/orders/507f1f77bcf86cd799439018
{
  "id": "order_507f1f77bcf86cd799439018",
  "status": "completed",
  "result": {
    "successful": ["507f1f77bcf86cd799439012", "507f1f77bcf86cd799439016"],
    "failed": [],
    "createdLoans": ["loan_507f1f77bcf86cd799439019", "loan_507f1f77bcf86cd799439020"]
  }
}
```

#### Krok 2: Powtórzenie tego samego zlecenia
```http
POST /api/v1/orders
Content-Type: application/json
Idempotency-Key: user13-bulk-loan-20250610-001
{
  "type": "bulk_loan",
  "userId": "507f1f77bcf86cd799439013",
  "bookIds": ["507f1f77bcf86cd799439012", "507f1f77bcf86cd799439016"]
}
```

#### Odpowiedź (identyczna jak poprzednio):
```http
HTTP/1.1 201 Created
Content-Type: application/json
Location: /api/v1/orders/507f1f77bcf86cd799439018
{
  "id": "order_507f1f77bcf86cd799439018",
  "status": "completed",
  "result": {
    "successful": ["507f1f77bcf86cd799439012", "507f1f77bcf86cd799439016"],
    "failed": [],
    "createdLoans": ["loan_507f1f77bcf86cd799439019", "loan_507f1f77bcf86cd799439020"]
  }
}
```

### 7.3 Scenariusz: Atomowa Operacja Batch

#### Żądanie:
```http
POST /api/v1/batch-operations/bulk-return
Content-Type: application/json
{
  "operations": [
    {
      "loanId": "loan_507f1f77bcf86cd799439019",
      "returnDate": "2025-06-10T16:30:00.000Z"
    },
    {
      "loanId": "loan_507f1f77bcf86cd799439020",
      "returnDate": "2025-06-10T16:30:00.000Z"
    }
  ],
  "executionMode": "atomic"
}
```

#### Odpowiedź (sukces):
```http
HTTP/1.1 200 OK
Content-Type: application/json
{
  "batchId": "batch_507f1f77bcf86cd799439025",
  "status": "completed",
  "totalOperations": 2,
  "successfulOperations": 2,
  "failedOperations": 0,
  "results": [
    {
      "operation": 1,
      "status": "success",
      "loanId": "loan_507f1f77bcf86cd799439019",
      "bookId": "507f1f77bcf86cd799439012",
      "updatedAvailableCopies": 3
    },
    {
      "operation": 2,
      "status": "success",
      "loanId": "loan_507f1f77bcf86cd799439020",
      "bookId": "507f1f77bcf86cd799439016",
      "updatedAvailableCopies": 5
    }
  ],
  "executedAt": "2025-06-10T16:30:00.000Z",
  "duration": "0.150s"
}
```

## 8. Mapowanie Wymagań na Implementację

| Wymaganie | Zasób | Implementacja |
|-----------|-------|---------------|
| **Proste zasoby CRUD** | `/authors/{id}` | Pełny CRUD z walidacją i ETag |
| **Zasoby-kolekcje ze stronicowaniem** | `/books` | Parametry page, limit, sort z metadanymi |
| **Warunková aktualizacja (Lost Update)** | `/books/{id}`, `/loans/{id}` | If-Match header z ETag validation |
| **POST once exactly** | `/orders` | Idempotency-Key header z TTL storage |
| **Zasoby-kontrolery** | `/batch-operations/*` | Atomowe operacje na wielu zasobach |

## 9. Podsumowanie Specyfikacji

### 9.1 Statystyki API
- **Liczba zasobów:** 8 głównych typów
- **Liczba endpointów:** 24 unikalne ścieżki
- **Obsługiwane metody HTTP:** GET, POST, PUT, PATCH, DELETE, HEAD
- **Formaty danych:** JSON (wejście i wyjście)
- **Mechanizmy bezpieczeństwa:** ETag, Idempotency-Key, Authorization

### 9.2 Zgodność z REST
- ✅ **Uniform Interface:** Konsystentne użycie metod HTTP
- ✅ **Stateless:** Każde żądanie zawiera pełny kontekst
- ✅ **Cacheable:** Nagłówki Cache-Control i ETag
- ✅ **Client-Server:** Wyraźny podział odpowiedzialności
- ✅ **Layered System:** Możliwość dodania proxy, cache, load balancer
- ✅ **HATEOAS:** Linki _links w odpowiedziach

Ta specyfikacja stanowi kompletną dokumentację tabelaryczną dla systemu REST zarządzania biblioteką, spełniającą wszystkie wymagania projektowe.": "2025-06-10T16:20:00.000Z",
  "path": "/api/v1/users",
  "method": "POST"
}
```

#### Błąd Konkurencji (412):
```json
{
  "error": {
    "code": "PRECONDITION_FAILED",
    "message": "Resource has been modified by another request",
    "details": {
      "currentEtag": "\"507f1f77bcf86cd799439012-1736515900000\"",
      "providedEtag": "\"507f1f77bcf86cd799439012-1736515800000\""
    }
  },
  "timestamp