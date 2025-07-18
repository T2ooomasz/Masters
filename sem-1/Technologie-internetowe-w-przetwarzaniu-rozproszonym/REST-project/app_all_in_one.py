from flask import Flask, request, jsonify, url_for
import hashlib
import uuid
from datetime import datetime
from functools import wraps

app = Flask(__name__)

# Globalne struktury danych w pamięci
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

def validate_author_data(data):
    """Waliduje dane autora"""
    if not data:
        return False, "Request body is required"
    
    if not isinstance(data, dict):
        return False, "Request body must be a JSON object"
    
    if 'name' not in data:
        return False, "Field 'name' is required"
    
    if not isinstance(data['name'], str) or not data['name'].strip():
        return False, "Field 'name' must be a non-empty string"
    
    return True, None

def create_error_response(message, code, details=None):
    """Tworzy standaryzowaną odpowiedź błędu"""
    error_data = {
        "error": message,
        "code": code
    }
    if details:
        error_data["details"] = details
    
    return jsonify(error_data), code

# =============================================================================
# CRUD OPERACJE DLA AUTORÓW
# =============================================================================

@app.route('/api/v1/authors', methods=['GET', 'POST'])
def authors_collection():
    """
    GET /authors - Zwraca listę wszystkich autorów
    POST /authors - Dodaje nowego autora
    """
    
    if request.method == 'GET':
        # Pobieranie parametrów stronicowania dla spójności z innymi endpointami
        try:
            page = int(request.args.get('page', 1))
            limit = int(request.args.get('limit', 10))
        except ValueError:
            return create_error_response("Invalid pagination parameters", 400,
                                       "Parameters 'page' and 'limit' must be positive integers")

        # Walidacja parametrów
        if page < 1:
            return create_error_response("Invalid page parameter", 400,
                                       "Parameter 'page' must be greater than 0")

        if limit < 1 or limit > 100:
            return create_error_response("Invalid limit parameter", 400,
                                       "Parameter 'limit' must be between 1 and 100")

        # Użycie tej samej logiki paginacji co dla książek
        result = paginate_data(authors, page, limit)
        response = jsonify(result)
        
        # Budowanie nagłówka Link dla paginacji, tak jak w /books
        links = []
        base_url = request.base_url
        pagination_info = result['pagination']

        if pagination_info['has_next']:
            next_url = f'{base_url}?page={page + 1}&limit={limit}'
            links.append(f'<{next_url}>; rel="next"')
            
        if pagination_info['has_previous']:
            prev_url = f'{base_url}?page={page - 1}&limit={limit}'
            links.append(f'<{prev_url}>; rel="prev"')
            
        if links:
            response.headers['Link'] = ', '.join(links)
            
        return response, 200
    
    elif request.method == 'POST':
        # Dodajemy nowego autora
        try:
            data = request.get_json()
        except Exception:
            return create_error_response("Invalid JSON", 400)
        
        # Walidacja danych
        is_valid, error_msg = validate_author_data(data)
        if not is_valid:
            return create_error_response("Validation error", 400, error_msg)
        
        # Tworzenie nowego autora
        author_id = str(uuid.uuid4())
        author = {
            "id": author_id,
            "name": data['name'].strip(),
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        
        # Dodawanie opcjonalnych pól
        if 'bio' in data and isinstance(data['bio'], str):
            author['bio'] = data['bio'].strip()
        
        if 'birth_year' in data and isinstance(data['birth_year'], int):
            author['birth_year'] = data['birth_year']
        
        # Generowanie ETag
        author['etag'] = generate_etag(author)
        
        # Zapisywanie w pamięci
        authors[author_id] = author
        
        response = jsonify(author)
        # Zgodnie ze standardem REST, dla 201 Created używamy nagłówka Location
        response.headers['Location'] = url_for('author_resource', author_id=author_id, _external=True)
        return response, 201

@etag_precondition_check(resource_collection=authors, required=True)
def update_author_put(author_id):
    """Obsługuje logikę dla PUT /authors/{id} z wymaganym ETag."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)
    
    is_valid, error_msg = validate_author_data(data)
    if not is_valid:
        return create_error_response("Validation error", 400, error_msg)
    
    current_author = authors[author_id]
    updated_author = {
        "id": author_id,
        "name": data['name'].strip(),
        "created_at": current_author['created_at'],
        "updated_at": datetime.now().isoformat()
    }
    
    if 'bio' in data: updated_author['bio'] = data.get('bio', '').strip()
    if 'birth_year' in data: updated_author['birth_year'] = data.get('birth_year')
    
    updated_author['etag'] = generate_etag(updated_author)
    authors[author_id] = updated_author
    
    response = jsonify(updated_author)
    response.headers['ETag'] = f'"{updated_author["etag"]}"'
    return response, 200

@etag_precondition_check(resource_collection=authors, required=False)
def update_author_patch(author_id):
    """Obsługuje logikę dla PATCH /authors/{id} z opcjonalnym ETag."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)
    
    if not data or not isinstance(data, dict):
        return create_error_response("Request body must be a non-empty JSON object", 400)

    current_author = authors[author_id].copy()
    validation_errors = []

    updatable_fields = {
        'name': lambda v: isinstance(v, str) and v.strip(),
        'bio': lambda v: isinstance(v, str) or v is None,
        'birth_year': lambda v: isinstance(v, int) or v is None
    }

    for field, value in data.items():
        if field in updatable_fields:
            if updatable_fields[field](value):
                if value is None:
                    current_author.pop(field, None)
                else:
                    current_author[field] = value.strip() if isinstance(value, str) else value
            else:
                validation_errors.append(f"Invalid value for field '{field}'")

    if validation_errors:
        return create_error_response("Validation error", 400, ", ".join(validation_errors))

    if 'name' not in current_author or not current_author.get('name'):
        return create_error_response("Validation error", 400, "Field 'name' cannot be removed or empty")

    current_author['updated_at'] = datetime.now().isoformat()
    current_author['etag'] = generate_etag(current_author)
    authors[author_id] = current_author

    response = jsonify(current_author)
    response.headers['ETag'] = f'"{current_author["etag"]}"'
    return response, 200

@etag_precondition_check(resource_collection=authors, required=False)
def delete_author(author_id):
    """Obsługuje logikę dla DELETE /authors/{id} z opcjonalnym ETag."""
    del authors[author_id]
    return '', 204

@app.route('/api/v1/authors/<author_id>', methods=['GET', 'PUT', 'PATCH', 'DELETE'])
def author_resource(author_id):
    """
    GET /authors/{id} - Zwraca informacje o autorze
    PUT /authors/{id} - Aktualizuje autora (pełna aktualizacja)
    PATCH /authors/{id} - Aktualizuje autora (częściowa aktualizacja)
    DELETE /authors/{id} - Usuwa autora
    """
    
    # Sprawdzamy czy autor istnieje
    if author_id not in authors:
        return create_error_response("Author not found", 404, 
                                   f"Author with id '{author_id}' does not exist")
    
    if request.method == 'GET':
        # Zwracamy informacje o autorze
        author = authors[author_id]
        response = jsonify(author)
        response.headers['ETag'] = f'"{author["etag"]}"'
        return response, 200
    
    elif request.method == 'PUT':
        return update_author_put(author_id)
    
    elif request.method == 'PATCH':
        return update_author_patch(author_id)
    
    elif request.method == 'DELETE':
        # Używamy tej samej logiki co dla książek, z opcjonalnym ETag
        # Wystarczy przenieść logikę do osobnej, udekorowanej funkcji
        return delete_author(author_id)

def validate_book_data(data):
    """Waliduje dane książki"""
    if not data:
        return False, "Request body is required"
    
    if not isinstance(data, dict):
        return False, "Request body must be a JSON object"
    
    required_fields = ['title', 'author_id']
    for field in required_fields:
        if field not in data:
            return False, f"Field '{field}' is required"
    
    if not isinstance(data['title'], str) or not data['title'].strip():
        return False, "Field 'title' must be a non-empty string"
    
    if not isinstance(data['author_id'], str) or not data['author_id'].strip():
        return False, "Field 'author_id' must be a non-empty string"
    
    # Sprawdzamy czy autor istnieje
    if data['author_id'] not in authors:
        return False, f"Author with id '{data['author_id']}' does not exist"
    
    # Walidacja opcjonalnych pól
    if 'copies' in data:
        if not isinstance(data['copies'], int) or data['copies'] < 0:
            return False, "Field 'copies' must be a non-negative integer"
    
    if 'isbn' in data:
        if not isinstance(data['isbn'], str) or not data['isbn'].strip():
            return False, "Field 'isbn' must be a non-empty string"
    
    if 'publication_year' in data:
        if not isinstance(data['publication_year'], int) or data['publication_year'] < 0:
            return False, "Field 'publication_year' must be a positive integer"
    
    return True, None

def paginate_data(data_dict, page, limit):
    """Implementuje stronicowanie dla słownika danych"""
    # Konwersja do listy i sortowanie po ID dla stabilnego porządku
    all_items = sorted(data_dict.values(), key=lambda x: x['id'])
    total = len(all_items)
    
    # Obliczanie indeksów
    start_index = (page - 1) * limit
    end_index = start_index + limit
    
    # Pobranie odpowiedniej strony
    page_items = all_items[start_index:end_index]
    
    # Obliczanie metadanych
    total_pages = (total + limit - 1) // limit  # Ceiling division
    has_next = page < total_pages
    has_prev = page > 1
    
    return {
        "data": page_items,
        "pagination": {
            "current_page": page,
            "per_page": limit,
            "total_items": total,
            "total_pages": total_pages,
            "has_next": has_next,
            "has_previous": has_prev
        }
    }

def etag_precondition_check(resource_collection, required=False):
    """
    Dekorator do sprawdzania nagłówka If-Match i ETag.
    :param resource_collection: Słownik przechowujący zasoby (np. `books` lub `authors`).
    :param required: Czy nagłówek If-Match jest wymagany.
    """
    def decorator(f):
        @wraps(f)
        def decorated_function(resource_id, *args, **kwargs):
            if_match_header = request.headers.get('If-Match')

            if required and not if_match_header:
                return create_error_response(
                    "Precondition Required", 428,
                    "If-Match header is required for this operation."
                )

            if if_match_header:
                provided_etag = if_match_header.strip('"')
                # Upewniamy się, że zasób istnieje przed próbą dostępu
                if resource_id in resource_collection:
                    current_etag = resource_collection[resource_id].get("etag")

                    if provided_etag != current_etag:
                        return create_error_response(
                            "Precondition Failed", 412,
                            "The resource has been modified by another client."
                        )
            
            return f(resource_id, *args, **kwargs)
        return decorated_function
    return decorator

def idempotent_post(f):
    """
    Dekorator zapewniający idempotencję dla operacji POST.
    Sprawdza nagłówek 'Idempotency-Key' i zwraca zapisaną odpowiedź, jeśli klucz był już użyty.
    """
    @wraps(f)
    def decorated_function(*args, **kwargs):
        idempotency_key = request.headers.get('Idempotency-Key')

        if idempotency_key and idempotency_key in idempotency_keys:
            # Zwrócenie poprzedniej odpowiedzi z cache
            cached_response = idempotency_keys[idempotency_key]
            return jsonify(cached_response["data"]), cached_response["status"]

        # Wywołanie oryginalnej funkcji, jeśli klucz jest nowy lub go nie ma
        response, status_code = f(*args, **kwargs)

        if idempotency_key and status_code in [200, 201, 202]:
            # Zapisanie odpowiedzi w cache tylko dla udanych operacji
            idempotency_keys[idempotency_key] = {"data": response.get_json(), "status": status_code}
        
        return response, status_code
    return decorated_function

# =============================================================================
# KOLEKCJA KSIĄŻEK ZE STRONICOWANIEM
# =============================================================================

@app.route('/api/v1/books', methods=['GET', 'POST'])
def books_collection():
    """
    GET /books - Zwraca listę książek ze stronicowaniem
    POST /books - Dodaje nową książkę
    """
    
    if request.method == 'GET':
        # Pobieranie parametrów stronicowania
        try:
            page = int(request.args.get('page', 1))
            limit = int(request.args.get('limit', 10))
        except ValueError:
            return create_error_response("Invalid pagination parameters", 400, 
                                       "Parameters 'page' and 'limit' must be positive integers")
        
        # Walidacja parametrów
        if page < 1:
            return create_error_response("Invalid page parameter", 400, 
                                       "Parameter 'page' must be greater than 0")
        
        if limit < 1 or limit > 100:
            return create_error_response("Invalid limit parameter", 400, 
                                       "Parameter 'limit' must be between 1 and 100")
        
        # Stronicowanie danych
        result = paginate_data(books, page, limit)
        
        # Dodawanie informacji o autorach do książek
        for book in result["data"]:
            if book['author_id'] in authors:
                book['author_name'] = authors[book['author_id']]['name']
            else:
                book['author_name'] = "Unknown Author"
        
        response = jsonify(result)
        
        # Budowanie nagłówka Link dla paginacji
        links = []
        base_url = request.base_url
        pagination_info = result['pagination']
        
        if pagination_info['has_next']:
            next_url = f'{base_url}?page={page + 1}&limit={limit}'
            links.append(f'<{next_url}>; rel="next"')
            
        if pagination_info['has_previous']:
            prev_url = f'{base_url}?page={page - 1}&limit={limit}'
            links.append(f'<{prev_url}>; rel="prev"')
            
        if links:
            response.headers['Link'] = ', '.join(links)
            
        return response, 200
    
    elif request.method == 'POST':
        # Dodawanie nowej książki
        try:
            data = request.get_json()
        except Exception:
            return create_error_response("Invalid JSON", 400)
        
        # Walidacja danych
        is_valid, error_msg = validate_book_data(data)
        if not is_valid:
            return create_error_response("Validation error", 400, error_msg)
        
        # Tworzenie nowej książki
        book_id = str(uuid.uuid4())
        book = {
            "id": book_id,
            "title": data['title'].strip(),
            "author_id": data['author_id'],
            "copies": data.get('copies', 1),
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        
        # Dodawanie opcjonalnych pól
        if 'isbn' in data:
            book['isbn'] = data['isbn'].strip()
        
        if 'publication_year' in data:
            book['publication_year'] = data['publication_year']
        
        if 'description' in data and isinstance(data['description'], str):
            book['description'] = data['description'].strip()
        
        # Generowanie ETag
        book['etag'] = generate_etag(book)
        
        # Zapisywanie w pamięci
        books[book_id] = book
        
        # Dodawanie nazwy autora do odpowiedzi
        book['author_name'] = authors[data['author_id']]['name']
        
        response = jsonify(book)
        # Zgodnie ze standardem REST, dla 201 Created używamy nagłówka Location
        response.headers['Location'] = url_for('book_resource', book_id=book_id, _external=True)
        return response, 201

@etag_precondition_check(resource_collection=books, required=True)
def update_book_put(book_id):
    """Obsługuje logikę dla PUT /books/{id} z wymaganym ETag."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    is_valid, error_msg = validate_book_data(data)
    if not is_valid:
        return create_error_response("Validation error", 400, error_msg)

    # Tworzenie nowej wersji książki z zachowaniem daty utworzenia
    current_book = books[book_id]
    updated_book = {
        "id": book_id,
        "title": data['title'].strip(),
        "author_id": data['author_id'],
        "copies": data.get('copies', 1),
        "created_at": current_book['created_at'],
        "updated_at": datetime.now().isoformat()
    }

    if 'isbn' in data: updated_book['isbn'] = data['isbn'].strip()
    if 'publication_year' in data: updated_book['publication_year'] = data['publication_year']
    if 'description' in data: updated_book['description'] = data['description'].strip()

    updated_book['etag'] = generate_etag(updated_book)
    books[book_id] = updated_book

    response = jsonify(updated_book)
    response.headers['ETag'] = f'"{updated_book["etag"]}"'
    return response, 200

@etag_precondition_check(resource_collection=books, required=False)
def update_book_patch(book_id):
    """Obsługuje logikę dla PATCH /books/{id} z opcjonalnym ETag."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    if not data or not isinstance(data, dict):
        return create_error_response("Request body must be a non-empty JSON object", 400)

    current_book = books[book_id].copy()
    validation_errors = []

    # Definiujemy pola, które można aktualizować i ich walidację
    updatable_fields = {
        'title': lambda v: isinstance(v, str) and v.strip(),
        'author_id': lambda v: isinstance(v, str) and v.strip() and v in authors,
        'copies': lambda v: isinstance(v, int) and v >= 0,
        'isbn': lambda v: isinstance(v, str) or v is None,
        'publication_year': lambda v: isinstance(v, int) or v is None,
        'description': lambda v: isinstance(v, str) or v is None
    }

    for field, value in data.items():
        if field in updatable_fields:
            if updatable_fields[field](value):
                if value is None:
                    current_book.pop(field, None)
                else:
                    current_book[field] = value.strip() if isinstance(value, str) else value
            else:
                validation_errors.append(f"Invalid value for field '{field}'")

    if validation_errors:
        return create_error_response("Validation error", 400, ", ".join(validation_errors))

    current_book['updated_at'] = datetime.now().isoformat()
    current_book['etag'] = generate_etag(current_book)
    books[book_id] = current_book

    response = jsonify(current_book)
    response.headers['ETag'] = f'"{current_book["etag"]}"'
    return response, 200

@etag_precondition_check(resource_collection=books, required=False)
def delete_book(book_id):
    """Obsługuje logikę dla DELETE /books/{id} z opcjonalnym ETag."""
    del books[book_id]
    return '', 204

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
        return update_book_put(book_id)
    
    elif request.method == 'PATCH':
        return update_book_patch(book_id)
    
    else:  # DELETE
        return delete_book(book_id)

# ==================== ZLECENIA - POST ONCE EXACTLY ====================

@idempotent_post
def create_order():
    """Tworzy nowe zlecenie (logika dla POST /orders)"""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    if not data or not isinstance(data, dict):
        return create_error_response("Invalid JSON payload", 400)

    # Walidacja danych wejściowych zamówienia
    book_id = data.get('bookId')
    quantity = data.get('quantity')

    if not book_id or book_id not in books:
        return create_error_response("Not Found", 404, f"Book with id '{book_id}' not found.")

    if not isinstance(quantity, int) or quantity <= 0:
        return create_error_response("Validation error", 400, "Field 'quantity' must be a positive integer.")

    # Logika biznesowa (np. sprawdzanie dostępności książek)
    if books[book_id]['copies'] < quantity:
        return create_error_response("Insufficient stock", 409, 
                                   f"Not enough copies of '{books[book_id]['title']}'. Available: {books[book_id]['copies']}")
    
    order_id = str(uuid.uuid4())
    order = {
        **data,
        "id": order_id,
        "status": "created",
        "created_at": datetime.now().isoformat()
    }
    # Zmniejszenie liczby dostępnych kopii
    books[book_id]['copies'] -= quantity
    orders[order_id] = order
    
    response = jsonify(order)
    response.headers['Location'] = url_for('order_resource', order_id=order_id, _external=True)
    return response, 201

@app.route('/api/v1/orders', methods=['GET', 'POST'])
def orders_collection():
    """Zlecenia z obsługą idempotencji"""
    if request.method == 'GET':
        # Użycie tej samej logiki paginacji co dla książek i autorów
        try:
            page = int(request.args.get('page', 1))
            limit = int(request.args.get('limit', 10))
        except ValueError:
            return create_error_response("Invalid pagination parameters", 400,
                                       "Parameters 'page' and 'limit' must be positive integers")

        if page < 1 or limit < 1 or limit > 100:
            return create_error_response("Invalid pagination parameters", 400,
                                       "Page must be > 0 and limit must be between 1 and 100")

        result = paginate_data(orders, page, limit)
        response = jsonify(result)

        # Budowanie nagłówka Link dla paginacji
        links = []
        base_url = request.base_url
        pagination_info = result['pagination']

        if pagination_info['has_next']:
            links.append(f'<{base_url}?page={page + 1}&limit={limit}>; rel="next"')
        if pagination_info['has_previous']:
            links.append(f'<{base_url}?page={page - 1}&limit={limit}>; rel="prev"')
        if links:
            response.headers['Link'] = ', '.join(links)
        return response, 200
    
    else:  # POST
        return create_order()

@app.route('/api/v1/orders/<order_id>', methods=['GET'])
def order_resource(order_id):
    """Zwraca informacje o pojedynczym zleceniu"""
    if order_id not in orders:
        return create_error_response("Order not found", 404,
                                     f"Order with id '{order_id}' does not exist")
    
    order = orders[order_id]
    response = jsonify(order)
    return response, 200


# ==================== KONTROLER - BULK UPDATE ====================

@idempotent_post
@app.route('/api/v1/batch/bulk-update', methods=['POST'])
def bulk_update():
    """
    Aktualizuje wiele książek w jednej operacji.
    Oczekuje JSON: {"bookIds": ["1", "2"], "updates": {"copies": 10}}
    """
    data = request.json
    book_ids = data.get('bookIds')
    updates = data.get('updates')

    # Walidacja wejścia
    if not isinstance(book_ids, list) or not isinstance(updates, dict):
        return create_error_response("Invalid payload format", 400)

    # Faza 1: Walidacja ("All or Nothing")
    # Sprawdzamy, czy wszystkie książki istnieją. Jeśli nie, cała operacja jest odrzucana.
    not_found_ids = [book_id for book_id in book_ids if book_id not in books]
    if not_found_ids:
        return create_error_response(
            "Precondition Failed", 412,
            f"Operation failed. The following books were not found: {not_found_ids}"
        )

    # Walidujemy pola i wartości w `updates` przed rozpoczęciem jakichkolwiek modyfikacji.
    updatable_fields = {
        'title': lambda v: isinstance(v, str) and v.strip(),
        'author_id': lambda v: isinstance(v, str) and v.strip() and v in authors,
        'copies': lambda v: isinstance(v, int) and v >= 0,
        'isbn': lambda v: isinstance(v, str) or v is None,
        'publication_year': lambda v: isinstance(v, int) or v is None,
        'description': lambda v: isinstance(v, str) or v is None
    }
    
    for field, value in updates.items():
        if field not in updatable_fields:
            return create_error_response(
                "Validation error", 400, f"Field '{field}' cannot be updated."
            )
        if not updatable_fieldsfield:
            return create_error_response(
                "Validation error", 400, f"Invalid value provided for field '{field}'."
            )

    # Faza 2: Wykonanie (Commit)
    # Jeśli walidacja przeszła pomyślnie, możemy bezpiecznie zaktualizować wszystkie zasoby.
    updated_books = []
    for book_id in book_ids:
        # Zastosuj bezpieczne aktualizacje i zaktualizuj metadane
        books[book_id].update(updates)
        books[book_id]['updated_at'] = datetime.now().isoformat()
        books[book_id]['etag'] = generate_etag(books[book_id])
        updated_books.append(books[book_id])

    # Zwracamy odpowiedź potwierdzającą sukces całej operacji
    response_data = {
        "status": "Completed successfully",
        "updated_count": len(updated_books),
        "updated_books": updated_books
    }
    return jsonify(response_data), 200

# =============================================================================
# HOOKS - MODYFIKACJA ODPOWIEDZI
# =============================================================================

@app.after_request
def add_self_link_header(response):
    """Dodaje nagłówek Link: rel="self" do każdej udanej odpowiedzi JSON."""
    # Chcemy dodawać linki tylko do udanych odpowiedzi (2xx) zwracających JSON
    if response.status_code >= 200 and response.status_code < 300 and \
       response.mimetype == 'application/json':
        
        # request.url zawiera pełny, absolutny URL żądania
        self_link = f'<{request.url}>; rel="self"'
        
        # Sprawdzamy, czy nagłówek Link już istnieje (np. dla paginacji)
        if 'Link' in response.headers:
            # Jeśli tak, dołączamy link "self" do istniejących
            response.headers['Link'] = f"{response.headers['Link']}, {self_link}"
        else:
            # W przeciwnym razie, ustawiamy nowy nagłówek
            response.headers['Link'] = self_link
            
    return response


# =============================================================================
# OBSŁUGA BŁĘDÓW
# =============================================================================

@app.errorhandler(404)
def not_found(error):
    return create_error_response("Not Found", 404, "The requested resource was not found")

@app.errorhandler(405)
def method_not_allowed(error):
    return create_error_response("Method Not Allowed", 405, 
                                "The method is not allowed for the requested URL")

@app.errorhandler(500)
def internal_server_error(error):
    return create_error_response("Internal Server Error", 500, 
                                "An unexpected error occurred")

# =============================================================================
# PRZYKŁADOWE DANE TESTOWE
# =============================================================================

def initialize_sample_data():
    """Inicjalizuje przykładowe dane dla testów"""
    sample_authors = [
        {
            "name": "J.R.R. Tolkien",
            "bio": "British author, philologist, and professor",
            "birth_year": 1892
        },
        {
            "name": "Andrzej Sapkowski",
            "bio": "Polish fantasy writer",
            "birth_year": 1948
        },
        {
            "name": "George R.R. Martin",
            "bio": "American novelist and short story writer",
            "birth_year": 1948
        }
    ]
    
    # Tworzenie autorów
    author_ids = []
    for author_data in sample_authors:
        author_id = str(uuid.uuid4())
        author = {
            "id": author_id,
            "name": author_data['name'],
            "bio": author_data['bio'],
            "birth_year": author_data['birth_year'],
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        author['etag'] = generate_etag(author)
        authors[author_id] = author
        author_ids.append(author_id)
    
    # Tworzenie przykładowych książek
    sample_books = [
        {"title": "My Random Book", "author_idx": 0, "copies": 5000, "isbn": "111-2223334445", "publication_year": 0000},
        {"title": "The Hobbit", "author_idx": 0, "copies": 5, "isbn": "978-0547928227", "publication_year": 1937},
        {"title": "The Fellowship of the Ring", "author_idx": 0, "copies": 3, "isbn": "978-0547928210", "publication_year": 1954},
        {"title": "The Two Towers", "author_idx": 0, "copies": 3, "isbn": "978-0547928203", "publication_year": 1954},
        {"title": "The Return of the King", "author_idx": 0, "copies": 3, "isbn": "978-0547928197", "publication_year": 1955},
        {"title": "The Last Wish", "author_idx": 1, "copies": 4, "isbn": "978-0316029186", "publication_year": 1993},
        {"title": "Sword of Destiny", "author_idx": 1, "copies": 4, "isbn": "978-0316389709", "publication_year": 1992},
        {"title": "Blood of Elves", "author_idx": 1, "copies": 2, "isbn": "978-0316029193", "publication_year": 1994},
        {"title": "Time of Contempt", "author_idx": 1, "copies": 2, "isbn": "978-0316219181", "publication_year": 1995},
        {"title": "A Game of Thrones", "author_idx": 2, "copies": 6, "isbn": "978-0553103540", "publication_year": 1996},
        {"title": "A Clash of Kings", "author_idx": 2, "copies": 5, "isbn": "978-0553108033", "publication_year": 1998},
        {"title": "A Storm of Swords", "author_idx": 2, "copies": 4, "isbn": "978-0553106633", "publication_year": 2000},
        {"title": "A Feast for Crows", "author_idx": 2, "copies": 3, "isbn": "978-0553801507", "publication_year": 2005},
        {"title": "A Dance with Dragons", "author_idx": 2, "copies": 3, "isbn": "978-0553801477", "publication_year": 2011},
        {"title": "Fire & Blood", "author_idx": 2, "copies": 2, "isbn": "978-1524796280", "publication_year": 2018},
        {"title": "The World of Ice & Fire", "author_idx": 2, "copies": 1, "isbn": "978-0553805444", "publication_year": 2014}
    ]
    
    for book_data in sample_books:
        book_id = str(uuid.uuid4())
        book = {
            "id": book_id,
            "title": book_data['title'],
            "author_id": author_ids[book_data['author_idx']],
            "copies": book_data['copies'],
            "isbn": book_data['isbn'],
            "publication_year": book_data['publication_year'],
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        book['etag'] = generate_etag(book)
        books[book_id] = book

# =============================================================================
# URUCHOMIENIE APLIKACJI
# =============================================================================

if __name__ == '__main__':
    # Inicjalizacja przykładowych danych
    initialize_sample_data()
    
    print("=== Library Management System - Authors, Books, Orders, Bulk Update ===")
    print("Dostępne endpointy (zgodnie z REST):")
    print("\n[AUTHORS]")
    print("  GET    /api/v1/authors             - Lista autorów")
    print("  POST   /api/v1/authors             - Dodaj autora")
    print("  GET    /api/v1/authors/{id}        - Info o autorze")
    print("  PUT    /api/v1/authors/{id}        - Aktualizacja autora (pełna)")
    print("  PATCH  /api/v1/authors/{id}        - Aktualizacja autora (częściowa)")
    print("  DELETE /api/v1/authors/{id}        - Usuń autora")

    print("\n[BOOKS]")
    print("  GET    /api/v1/books               - Lista książek (stronicowanie)")
    print("  POST   /api/v1/books               - Dodaj książkę")
    print("  GET    /api/v1/books/{id}          - Info o książce")
    print("  PUT    /api/v1/books/{id}          - Aktualizacja książki (ETag)")
    print("  PATCH  /api/v1/books/{id}          - Częściowa aktualizacja książki (ETag)")
    print("  DELETE /api/v1/books/{id}          - Usuń książkę")

    print("\n[ORDERS]")
    print("  GET    /api/v1/orders              - Lista zleceń")
    print("  GET    /api/v1/orders/{id}         - Info o zleceniu")
    print("  POST   /api/v1/orders              - Utwórz zlecenie (idempotentne)")

    print("\n[BULK UPDATE]")
    print("  POST   /api/v1/batch/bulk-update   - Zbiorcza aktualizacja książek")

    print(f"\nDane przykładowe: {len(authors)} autorów, {len(books)} książek")
    print("\nSerwer uruchomiony na http://localhost:5000")
    print("=" * 65)
    
    app.run(debug=True, host='0.0.0.0', port=5000)