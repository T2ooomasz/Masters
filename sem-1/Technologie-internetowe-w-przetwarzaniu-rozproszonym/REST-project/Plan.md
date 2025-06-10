# Uproszczony Plan REST API - System Biblioteki

## 1. Koncepcja
**System zarządzania biblioteką** - uproszczony do minimum wymaganego przez zadanie.

## 2. Hierarchia Zasobów

```
/api/v1/
├── /authors                    # Kolekcja autorów (CRUD)
├── /authors/{id}              # Pojedynczy autor (CRUD)
├── /books                     # Kolekcja książek (stronicowanie)
├── /books/{id}                # Pojedyncza książka (Lost Update - ETag)
├── /orders                    # Zlecenia (POST once exactly)
└── /batch/bulk-update         # Kontroler (aktualizacja wielu książek)
```

## 3. Implementacja Wymagań

### 3.1 CRUD - Autorzy
- **Zasób**: `/authors/{id}`
- **Operacje**: GET, POST (na kolekcji), PUT, DELETE

### 3.2 Kolekcja ze stronicowaniem - Książki
- **Zasób**: `/books`
- **Parametry**: `?page=1&limit=10&sort=title`

### 3.3 Lost Update Problem - Książki
- **Zasób**: `/books/{id}`
- **Mechanizm**: ETag w nagłówku `If-Match`

### 3.4 POST once exactly - Zlecenia
- **Zasób**: `/orders`
- **Mechanizm**: `Idempotency-Key` w nagłówku

### 3.5 Kontroler - Batch Update
- **Zasób**: `/batch/bulk-update`
- **Funkcja**: Atomowa aktualizacja wielu książek

## 4. Formaty Danych

### Autor
```json
{
  "id": "auth123",
  "name": "J.R.R. Tolkien",
  "birthYear": 1892,
  "etag": "abc123"
}
```

### Książka
```json
{
  "id": "book123",
  "title": "Hobbit",
  "authorId": "auth123",
  "copies": 5,
  "available": 3,
  "etag": "def456"
}
```

### Zlecenie
```json
{
  "id": "order123",
  "bookId": "book123",
  "action": "reserve",
  "timestamp": "2025-06-11T10:00:00Z"
}
```

## 5. Dokumentacja Usługi

| URI | GET | POST | PUT | PATCH | DELETE |
|-----|-----|------|-----|-------|--------|
| /authors | lista autorów | dodaj autora | X | X | X |
| /authors/{id} | info o autorze | X | aktualizacja | aktualizacja częściowa | usunięcie |
| /books | lista książek/stronicowanie | dodaj książkę | X | X | X |
| /books/{id} | info o książce | X | aktualizacja (ETag) | aktualizacja częściowa (ETag) | usunięcie |
| /orders | lista zleceń | utwórz zlecenie (idempotent) | X | X | X |
| /batch/bulk-update | X | aktualizuj wiele książek | X | X | X |

## 6. Kluczowe Mechanizmy

### ETag (Lost Update Prevention)
```http
PUT /books/123
If-Match: "etag-value"
```

### Idempotency Key
```http
POST /orders
Idempotency-Key: "unique-key-123"
```

### Stronicowanie
```http
GET /books?page=2&limit=5
```

### Batch Operation
```http
POST /batch/bulk-update
{
  "bookIds": ["book1", "book2"],
  "updates": {"available": 0}
}
```

## 7. Plan Implementacji

1. **Setup** - FastAPI + MongoDB
2. **Modele** - Author, Book, Order
3. **CRUD** - Podstawowe operacje
4. **Stronicowanie** - Dla kolekcji książek
5. **ETag** - Dla aktualizacji książek
6. **Idempotency** - Dla zleceń
7. **Batch** - Kontroler bulk-update
8. **Testy** - Postman Collection

## 8. Prezentacja

**Scenariusz demonstracji:**
1. CRUD na autorach
2. Stronicowanie książek
3. Test ETag (Lost Update)
4. Test Idempotency
5. Batch operation