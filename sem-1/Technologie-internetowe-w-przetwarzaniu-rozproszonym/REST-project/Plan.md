# Maksymalnie Uproszczony Plan REST API - System Biblioteki

## 1. Koncepcja
**System zarządzania biblioteką** - implementacja w pamięci bez bazy danych.

## 2. Technologia
- **Backend**: Python + Flask (pojedynczy plik)
- **Dane**: Słowniki Python w pamięci
- **Serwer**: Flask development server

## 3. Hierarchia Zasobów

```
/api/v1/
├── /authors                    # Kolekcja autorów (CRUD)
├── /authors/{id}              # Pojedynczy autor (CRUD)
├── /books                     # Kolekcja książek (stronicowanie)
├── /books/{id}                # Pojedyncza książka (Lost Update - ETag)
├── /orders                    # Zlecenia (POST once exactly)
└── /bulk-update               # Kontroler (aktualizacja wielu książek)
```

## 4. Struktura Danych w Pamięci

```python
# Globalne słowniki
authors = {}
books = {}
orders = {}
idempotency_keys = {}  # TTL: restart aplikacji
etags = {}             # Wersjonowanie
```

## 5. Implementacja Wymagań

### 5.1 CRUD - Autorzy (w pamięci)
```python
authors = {
    "1": {"id": "1", "name": "Tolkien", "etag": "v1"},
    "2": {"id": "2", "name": "Sapkowski", "etag": "v1"}
}
```

### 5.2 Kolekcja ze stronicowaniem - Książki
```python
# Stronicowanie przez slice()
def paginate(data, page, limit):
    start = (page - 1) * limit
    end = start + limit
    return list(data.values())[start:end]
```

### 5.3 Lost Update Problem - ETag
```python
# Generator ETag
import hashlib
def generate_etag(data):
    return hashlib.md5(str(data).encode()).hexdigest()[:8]
```

### 5.4 POST once exactly - Idempotency Key
```python
idempotency_keys = {}  # {key: response}
```

### 5.5 Kontroler - Bulk Update
```python
def bulk_update(book_ids, updates):
    # Atomowa aktualizacja w pamięci
    for book_id in book_ids:
        if book_id in books:
            books[book_id].update(updates)
```

## 6. Przykładowa Implementacja (app.py)

```python
from flask import Flask, request, jsonify
import hashlib
import uuid

app = Flask(__name__)

# Dane w pamięci
authors = {}
books = {}
orders = {}
idempotency_keys = {}

def generate_etag(data):
    return hashlib.md5(str(data).encode()).hexdigest()[:8]

# CRUD - Autorzy
@app.route('/authors', methods=['GET', 'POST'])
def authors_collection():
    if request.method == 'GET':
        return jsonify(list(authors.values()))
    else:  # POST
        data = request.json
        author_id = str(uuid.uuid4())
        author = {**data, "id": author_id, "etag": generate_etag(data)}
        authors[author_id] = author
        return jsonify(author), 201

@app.route('/authors/<id>', methods=['GET', 'PUT', 'DELETE'])
def author_resource(id):
    if id not in authors:
        return '', 404
    
    if request.method == 'GET':
        return jsonify(authors[id])
    elif request.method == 'PUT':
        data = request.json
        authors[id] = {**data, "id": id, "etag": generate_etag(data)}
        return jsonify(authors[id])
    else:  # DELETE
        del authors[id]
        return '', 204

# Stronicowanie - Książki
@app.route('/books', methods=['GET'])
def books_collection():
    page = int(request.args.get('page', 1))
    limit = int(request.args.get('limit', 10))
    
    all_books = list(books.values())
    start = (page - 1) * limit
    end = start + limit
    
    return jsonify({
        "data": all_books[start:end],
        "page": page,
        "limit": limit,
        "total": len(all_books)
    })

# Lost Update - ETag
@app.route('/books/<id>', methods=['PUT'])
def book_update(id):
    if id not in books:
        return '', 404
    
    if_match = request.headers.get('If-Match')
    if if_match != books[id]['etag']:
        return '', 412  # Precondition Failed
    
    data = request.json
    books[id] = {**data, "id": id, "etag": generate_etag(data)}
    return jsonify(books[id])

# POST once exactly - Idempotency
@app.route('/orders', methods=['POST'])
def create_order():
    idempotency_key = request.headers.get('Idempotency-Key')
    
    if idempotency_key in idempotency_keys:
        return idempotency_keys[idempotency_key]
    
    data = request.json
    order_id = str(uuid.uuid4())
    order = {**data, "id": order_id}
    orders[order_id] = order
    
    response = jsonify(order), 201
    if idempotency_key:
        idempotency_keys[idempotency_key] = response
    
    return response

# Kontroler - Bulk Update
@app.route('/bulk-update', methods=['POST'])
def bulk_update():
    data = request.json
    book_ids = data.get('bookIds', [])
    updates = data.get('updates', {})
    
    updated = []
    for book_id in book_ids:
        if book_id in books:
            books[book_id].update(updates)
            books[book_id]['etag'] = generate_etag(books[book_id])
            updated.append(books[book_id])
    
    return jsonify({"updated": updated})

if __name__ == '__main__':
    # Przykładowe dane
    books["1"] = {"id": "1", "title": "Hobbit", "copies": 5, "etag": "v1"}
    books["2"] = {"id": "2", "title": "LOTR", "copies": 3, "etag": "v1"}
    
    app.run(debug=True)
```

## 7. Dokumentacja Usługi

| URI | GET | POST | PUT | PATCH | DELETE |
|-----|-----|------|-----|-------|--------|
| /authors | lista autorów | dodaj autora | X | X | X |
| /authors/{id} | info o autorze | X | aktualizacja | aktualizacja częściowa | usunięcie |
| /books | lista książek/stronicowanie | dodaj książkę | X | X | X |
| /books/{id} | info o książce | X | aktualizacja (ETag) | aktualizacja częściowa (ETag) | usunięcie |
| /orders | lista zleceń | utwórz zlecenie (idempotent) | X | X | X |
| /batch/bulk-update | X | aktualizuj wiele książek | X | X | X |

## 8. Uruchomienie

```bash
pip install flask
python app.py
# Serwer na http://localhost:5000
```

## 9. Testowanie w Postman

```bash
# CRUD - Autor
POST /authors {"name": "Tolkien"}
GET /authors/1
PUT /authors/1 {"name": "J.R.R. Tolkien"}
DELETE /authors/1

# Stronicowanie
GET /books?page=1&limit=2

# Lost Update
GET /books/1  # Pobierz ETag
PUT /books/1 + Header: If-Match: "v1"

# Idempotency
POST /orders + Header: Idempotency-Key: "key123"

# Bulk Update
POST /bulk-update {"bookIds": ["1","2"], "updates": {"copies": 10}}
```