from flask import Flask, request, jsonify
import hashlib
import uuid
from datetime import datetime

app = Flask(__name__)

# Dane w pamięci
authors = {}
books = {}
orders = {}
idempotency_keys = {}

def generate_etag(data):
    """Generuje ETag na podstawie danych zasobu"""
    content = str(sorted(data.items())) if isinstance(data, dict) else str(data)
    return hashlib.md5(content.encode()).hexdigest()[:16]

def create_book_with_etag(book_data, book_id=None):
    """Tworzy książkę z automatycznym ETag"""
    if book_id is None:
        book_id = str(uuid.uuid4())
    
    # Usuwamy etag z danych wejściowych jeśli istnieje
    clean_data = {k: v for k, v in book_data.items() if k != 'etag'}
    
    book = {
        **clean_data,
        "id": book_id,
        "updated_at": datetime.now().isoformat()
    }
    
    # Generujemy ETag na podstawie czystych danych
    book["etag"] = generate_etag(book)
    return book

# ==================== AUTORZY - CRUD ====================

@app.route('/api/v1/authors', methods=['GET', 'POST'])
def authors_collection():
    """Kolekcja autorów - lista i tworzenie"""
    if request.method == 'GET':
        return jsonify({
            "data": list(authors.values()),
            "total": len(authors)
        })
    
    else:  # POST
        data = request.json
        if not data or 'name' not in data:
            return jsonify({"error": "Name is required"}), 400
        
        author_id = str(uuid.uuid4())
        author = {
            **data,
            "id": author_id,
            "created_at": datetime.now().isoformat(),
            "etag": generate_etag(data)
        }
        authors[author_id] = author
        
        response = jsonify(author)
        response.headers['ETag'] = f'"{author["etag"]}"'
        return response, 201

@app.route('/api/v1/authors/<author_id>', methods=['GET', 'PUT', 'PATCH', 'DELETE'])
def author_resource(author_id):
    """Pojedynczy autor - operacje CRUD"""
    if author_id not in authors:
        return jsonify({"error": "Author not found"}), 404
    
    if request.method == 'GET':
        author = authors[author_id]
        response = jsonify(author)
        response.headers['ETag'] = f'"{author["etag"]}"'
        return response
    
    elif request.method == 'PUT':
        data = request.json
        if not data or 'name' not in data:
            return jsonify({"error": "Name is required"}), 400
        
        author = {
            **data,
            "id": author_id,
            "updated_at": datetime.now().isoformat(),
            "etag": generate_etag(data)
        }
        authors[author_id] = author
        
        response = jsonify(author)
        response.headers['ETag'] = f'"{author["etag"]}"'
        return response
    
    elif request.method == 'PATCH':
        data = request.json
        if not data:
            return jsonify({"error": "No data provided"}), 400
        
        # Aktualizacja częściowa
        current_author = authors[author_id].copy()
        current_author.update(data)
        current_author["updated_at"] = datetime.now().isoformat()
        current_author["etag"] = generate_etag(current_author)
        
        authors[author_id] = current_author
        
        response = jsonify(current_author)
        response.headers['ETag'] = f'"{current_author["etag"]}"'
        return response
    
    else:  # DELETE
        del authors[author_id]
        return '', 204

# ==================== KSIĄŻKI - LOST UPDATE PROBLEM ====================

@app.route('/api/v1/books', methods=['GET', 'POST'])
def books_collection():
    """Kolekcja książek ze stronicowaniem"""
    if request.method == 'GET':
        page = int(request.args.get('page', 1))
        limit = int(request.args.get('limit', 10))
        
        if page < 1 or limit < 1:
            return jsonify({"error": "Page and limit must be positive"}), 400
        
        all_books = list(books.values())
        total = len(all_books)
        start = (page - 1) * limit
        end = start + limit
        
        return jsonify({
            "data": all_books[start:end],
            "pagination": {
                "page": page,
                "limit": limit,
                "total": total,
                "pages": (total + limit - 1) // limit if total > 0 else 0
            }
        })
    
    else:  # POST
        data = request.json
        if not data or 'title' not in data:
            return jsonify({"error": "Title is required"}), 400
        
        book = create_book_with_etag(data)
        books[book["id"]] = book
        
        response = jsonify(book)
        response.headers['ETag'] = f'"{book["etag"]}"'
        return response, 201

@app.route('/api/v1/books/<book_id>', methods=['GET', 'PUT', 'PATCH', 'DELETE'])
def book_resource(book_id):
    """Pojedyncza książka z obsługą ETag i Lost Update Problem"""
    if book_id not in books:
        return jsonify({"error": "Book not found"}), 404
    
    if request.method == 'GET':
        book = books[book_id]
        response = jsonify(book)
        response.headers['ETag'] = f'"{book["etag"]}"'
        return response
    
    elif request.method == 'PUT':
        # Sprawdzenie warunku If-Match dla ETag
        if_match = request.headers.get('If-Match')
        if not if_match:
            return jsonify({
                "error": "If-Match header is required for PUT operations",
                "message": "To prevent lost updates, include If-Match header with current ETag"
            }), 428  # Precondition Required
        
        # Usunięcie cudzysłowów z ETag jeśli istnieją
        if_match = if_match.strip('"')
        current_etag = books[book_id]["etag"]
        
        if if_match != current_etag:
            return jsonify({
                "error": "Precondition failed",
                "message": "The resource has been modified by another client",
                "current_etag": current_etag,
                "provided_etag": if_match
            }), 412  # Precondition Failed
        
        data = request.json
        if not data or 'title' not in data:
            return jsonify({"error": "Title is required"}), 400
        
        # Tworzenie nowej wersji książki
        updated_book = create_book_with_etag(data, book_id)
        books[book_id] = updated_book
        
        response = jsonify(updated_book)
        response.headers['ETag'] = f'"{updated_book["etag"]}"'
        return response
    
    elif request.method == 'PATCH':
        # Sprawdzenie warunku If-Match dla ETag (opcjonalne dla PATCH)
        if_match = request.headers.get('If-Match')
        if if_match:
            if_match = if_match.strip('"')
            current_etag = books[book_id]["etag"]
            
            if if_match != current_etag:
                return jsonify({
                    "error": "Precondition failed",
                    "message": "The resource has been modified by another client",
                    "current_etag": current_etag,
                    "provided_etag": if_match
                }), 412
        
        data = request.json
        if not data:
            return jsonify({"error": "No data provided"}), 400
        
        # Aktualizacja częściowa
        current_book = books[book_id].copy()
        current_book.update(data)
        updated_book = create_book_with_etag(current_book, book_id)
        books[book_id] = updated_book
        
        response = jsonify(updated_book)
        response.headers['ETag'] = f'"{updated_book["etag"]}"'
        return response
    
    else:  # DELETE
        # Opcjonalne sprawdzenie ETag dla DELETE
        if_match = request.headers.get('If-Match')
        if if_match:
            if_match = if_match.strip('"')
            current_etag = books[book_id]["etag"]
            
            if if_match != current_etag:
                return jsonify({
                    "error": "Precondition failed",
                    "message": "The resource has been modified by another client"
                }), 412
        
        del books[book_id]
        return '', 204

# ==================== ZLECENIA - POST ONCE EXACTLY ====================

@app.route('/api/v1/orders', methods=['GET', 'POST'])
def orders_collection():
    """Zlecenia z obsługą idempotencji"""
    if request.method == 'GET':
        return jsonify({
            "data": list(orders.values()),
            "total": len(orders)
        })
    
    else:  # POST
        # Sprawdzenie klucza idempotencji
        idempotency_key = request.headers.get('Idempotency-Key')
        
        if idempotency_key and idempotency_key in idempotency_keys:
            # Zwrócenie poprzedniej odpowiedzi
            cached_response = idempotency_keys[idempotency_key]
            response = jsonify(cached_response["data"])
            return response, cached_response["status"]
        
        data = request.json
        if not data:
            return jsonify({"error": "Order data is required"}), 400
        
        order_id = str(uuid.uuid4())
        order = {
            **data,
            "id": order_id,
            "status": "created",
            "created_at": datetime.now().isoformat()
        }
        orders[order_id] = order
        
        # Zapisanie w cache idempotencji
        if idempotency_key:
            idempotency_keys[idempotency_key] = {
                "data": order, # !
                "status": 201
            }
        
        return jsonify(order), 201

# ==================== KONTROLER - BULK UPDATE ====================

@app.route('/api/v1/bulk-update', methods=['POST'])
def bulk_update():
    """Kontroler do atomowej aktualizacji wielu książek"""
    data = request.json
    if not data:
        return jsonify({"error": "Request data is required"}), 400
    
    book_ids = data.get('bookIds', [])
    updates = data.get('updates', {})
    
    if not book_ids or not updates:
        return jsonify({"error": "bookIds and updates are required"}), 400
    
    # Sprawdzenie czy wszystkie książki istnieją
    missing_books = [book_id for book_id in book_ids if book_id not in books]
    if missing_books:
        return jsonify({
            "error": "Some books not found",
            "missing_books": missing_books
        }), 404
    
    # Atomowa aktualizacja
    updated_books = []
    for book_id in book_ids:
        current_book = books[book_id].copy()
        current_book.update(updates)
        updated_book = create_book_with_etag(current_book, book_id)
        books[book_id] = updated_book
        updated_books.append(updated_book)
    
    return jsonify({
        "message": f"Successfully updated {len(updated_books)} books",
        "updated_books": updated_books
    })

# ==================== OBSŁUGA BŁĘDÓW ====================

@app.errorhandler(404)
def not_found(error):
    return jsonify({"error": "Resource not found"}), 404

@app.errorhandler(400)
def bad_request(error):
    return jsonify({"error": "Bad request"}), 400

@app.errorhandler(500)
def internal_error(error):
    return jsonify({"error": "Internal server error"}), 500

# ==================== INICJALIZACJA DANYCH PRZYKŁADOWYCH ====================

def init_sample_data():
    """Inicjalizacja przykładowych danych"""
    
    # Przykładowi autorzy
    tolkien_data = {"name": "J.R.R. Tolkien", "birth_year": 1892}
    tolkien = create_book_with_etag(tolkien_data, "author-1")
    authors["author-1"] = tolkien
    
    sapkowski_data = {"name": "Andrzej Sapkowski", "birth_year": 1948}
    sapkowski = create_book_with_etag(sapkowski_data, "author-2")
    authors["author-2"] = sapkowski
    
    # Przykładowe książki
    hobbit_data = {
        "title": "Hobbit",
        "author_id": "author-1",
        "copies": 5,
        "available": 3,
        "isbn": "978-0547928227"
    }
    hobbit = create_book_with_etag(hobbit_data, "book-1")
    books["book-1"] = hobbit
    
    lotr_data = {
        "title": "The Lord of the Rings",
        "author_id": "author-1", 
        "copies": 3,
        "available": 1,
        "isbn": "978-0544003415"
    }
    lotr = create_book_with_etag(lotr_data, "book-2")
    books["book-2"] = lotr
    
    witcher_data = {
        "title": "Wiedźmin",
        "author_id": "author-2",
        "copies": 7,
        "available": 4,
        "isbn": "978-8375780611"
    }
    witcher = create_book_with_etag(witcher_data, "book-3")
    books["book-3"] = witcher

# ==================== ENDPOINTY POMOCNICZE ====================

@app.route('/api/v1/status', methods=['GET'])
def api_status():
    """Status API"""
    return jsonify({
        "status": "running",
        "version": "1.0.0",
        "timestamp": datetime.now().isoformat(),
        "resources": {
            "authors": len(authors),
            "books": len(books),
            "orders": len(orders)
        }
    })

@app.route('/api/v1', methods=['GET'])
def api_info():
    """Informacje o API"""
    return jsonify({
        "name": "Library Management API",
        "version": "1.0.0",
        "description": "REST API demonstrating CRUD, pagination, ETag, idempotency, and bulk operations",
        "endpoints": {
            "authors": "/api/v1/authors",
            "books": "/api/v1/books", 
            "orders": "/api/v1/orders",
            "bulk_update": "/api/v1/bulk-update",
            "status": "/api/v1/status"
        }
    })

if __name__ == '__main__':
    init_sample_data()
    print("🚀 Library REST API Server Starting...")
    print("📚 Sample data initialized")
    print("🔗 API available at: http://localhost:5000/api/v1")
    print("📖 Test Lost Update Problem with ETag on /api/v1/books/{id}")
    app.run(debug=True, host='0.0.0.0', port=5000)