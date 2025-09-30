import hashlib
import uuid
from datetime import datetime
from functools import wraps
from flask import jsonify, request

# Używamy importu relatywnego, aby odwołać się do modułów w tym samym pakiecie
from .data import authors, books, idempotency_keys

def generate_etag(data):
    """Generuje ETag na podstawie danych zasobu"""
    content = str(sorted(data.items())) if isinstance(data, dict) else str(data)
    return hashlib.md5(content.encode()).hexdigest()[:16]

'''def etag_precondition_check(resource_collection, required=False):
    """Dekorator do sprawdzania warunków wstępnych ETag (If-Match, If-None-Match)."""
    def decorator(func):
        @wraps(func)
        def wrapper(book_id=None, author_id=None):
            resource_id = book_id if book_id else author_id
            if resource_id not in resource_collection:
                return create_error_response("Not Found", 404, f"Resource with id '{resource_id}' not found.")

            resource = resource_collection[resource_id]
            current_etag = f'"{resource["etag"]}"'

            if_match = request.headers.get('If-Match')
            if_none_match = request.headers.get('If-None-Match')

            if required and not if_match:
                return create_error_response("Precondition Required", 428, "If-Match header is required for this operation.")

            if if_match and if_match != current_etag:
                return create_error_response("Precondition Failed", 412, "ETag does not match the current version of the resource.")

            if if_none_match and if_none_match == current_etag:
                return jsonify(resource), 304  # Not Modified

            return func(book_id=book_id, author_id=author_id)
        return wrapper
    return decorator'''

def etag_precondition_check(resource_collection, required=False):
    """Dekorator do sprawdzania warunków wstępnych ETag (If-Match, If-None-Match)."""
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            # Dynamiczne wyciąganie resource_id z kwargs (dla authors lub books)
            resource_id = kwargs.get('author_id') or kwargs.get('book_id')
            if not resource_id:
                # Fallback: jeśli nie ma w kwargs, sprawdź args (rzadki przypadek, ale na wszelki wypadek)
                if args:
                    resource_id = args[0]  # Pierwszy arg z route to zazwyczaj ID

            if resource_id not in resource_collection:
                return create_error_response("Not Found", 404, f"Resource with id '{resource_id}' not found.")

            resource = resource_collection[resource_id]
            current_etag = f'"{resource["etag"]}"'

            if_match = request.headers.get('If-Match')
            if_none_match = request.headers.get('If-None-Match')

            if required and not if_match:
                return create_error_response("Precondition Required", 428, "If-Match header is required for this operation.")

            if if_match and if_match != current_etag:
                return create_error_response("Precondition Failed", 412, "ETag does not match the current version of the resource.")

            if if_none_match and if_none_match == current_etag:
                return jsonify(resource), 304  # Not Modified

            # Przekazuj wszystkie args/kwargs dalej – bez narzucania book_id/author_id
            return func(*args, **kwargs)
        return wrapper
    return decorator

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
    paginated_items = all_items[start_index:end_index]

    # Informacje o paginacji
    pagination_info = {
        'total_items': total,
        'total_pages': (total + limit - 1) // limit,
        'current_page': page,
        'page_size': limit,
        'has_next': end_index < total,
        'has_previous': start_index > 0
    }

    return {
        "data": paginated_items,
        "pagination": pagination_info
    }

def idempotent(func):
    """Dekorator do obsługi idempotencji operacji POST"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        idempotency_key = request.headers.get('Idempotency-Key')
        if not idempotency_key:
            err_response = create_error_response("Idempotency-Key header is required", 400)
            return err_response, 400  # Zawsze tuple dla spójności

        if idempotency_key in idempotency_keys:
            cached_data, status_code = idempotency_keys[idempotency_key]
            cached_response = jsonify(cached_data)
            return cached_response, status_code

        # Wywołaj func i obsłuż różne formaty zwrotów
        func_result = func(*args, **kwargs)
        if isinstance(func_result, tuple):
            response, status_code = func_result
        else:
            # Jeśli single Response (np. stary kod), załóż 200
            response = func_result
            status_code = getattr(response, 'status_code', 200)
        
        # Zapisuj tylko przy sukcesie
        if 200 <= status_code < 300:
            cache_data = response.get_json() if hasattr(response, 'get_json') else {}
            idempotency_keys[idempotency_key] = (cache_data, status_code)
            
        return response, status_code
    return wrapper

'''def idempotent(func):
    """Dekorator do obsługi idempotencji operacji POST"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        idempotency_key = request.headers.get('Idempotency-Key')
        if not idempotency_key:
            return create_error_response("Idempotency-Key header is required", 400), 400
        if idempotency_key in idempotency_keys:
            response, status_code = idempotency_keys[idempotency_key]
            return jsonify(response), status_code

        response, status_code = func(*args, **kwargs)
        
        # Zapisujemy odpowiedź tylko jeśli operacja się powiodła (status 2xx)
        if 200 <= status_code < 300:
            idempotency_keys[idempotency_key] = (response.get_json(), status_code)
            
        return response, status_code
    return wrapper'''

def idempotent_patch(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        resource_id = kwargs.get('author_id') or kwargs.get('book_id')  # Dynamicznie
        idempotency_key = request.headers.get('Idempotency-Key')
        full_key = f"patch_{resource_id}_{idempotency_key}" if idempotency_key else None
        
        if full_key and full_key in idempotency_keys:
            response_data, status_code = idempotency_keys[full_key]
            return jsonify(response_data), status_code
        
        response, status_code = func(*args, **kwargs)
        
        if idempotency_key and 200 <= status_code < 300:
            idempotency_keys[full_key] = (response.get_json(), status_code)
        
        return response, status_code
    return wrapper

# Wraper wymagający idempotent key - ten wyżej obsługuje normalnie jak nie ma takiego header
'''def idempotent_patch(func):
    """Dekorator do obsługi idempotencji operacji PATCH (wymaga Idempotency-Key)."""
    @wraps(func)
    def wrapper(*args, **kwargs):
        # Dynamiczne wyciąganie resource_id (dla authors/books)
        resource_id = kwargs.get('author_id') or kwargs.get('book_id')
        if not resource_id:
            return create_error_response("Resource ID is required", 400)

        idempotency_key = request.headers.get('Idempotency-Key')
        if not idempotency_key:  # NOWE: Wymagaj klucz, jak w POST
            return create_error_response("Idempotency-Key header is required", 400)

        full_key = f"patch_{resource_id}_{idempotency_key}"  # Unikalny: typ + id + key
        if full_key in idempotency_keys:
            response_data, status_code = idempotency_keys[full_key]
            return jsonify(response_data), status_code

        # Przetwórz żądanie
        response, status_code = func(*args, **kwargs)
        
        # Zapisz w cache tylko przy sukcesie
        if 200 <= status_code < 300:
            response_data = response.get_json() if hasattr(response, 'get_json') else {}
            idempotency_keys[full_key] = (response_data, status_code)
            
        return response, status_code
    return wrapper
'''
def validate_batch_update_data(data):
    """Waliduje dane dla operacji wsadowej aktualizacji książek"""
    if not isinstance(data, dict):
        return False, "Request body must be a JSON object"

    book_ids = data.get('bookIds')
    updates = data.get('updates')

    if not isinstance(book_ids, list) or not book_ids:
        return False, "Field 'bookIds' must be a non-empty list"

    if not isinstance(updates, dict) or not updates:
        return False, "Field 'updates' must be a non-empty dictionary"

    # Sprawdzamy, czy wszystkie ID książek istnieją
    for book_id in book_ids:
        if book_id not in books:
            return False, f"Book with id '{book_id}' not found"

    # Walidujemy pola do aktualizacji
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
            return False, f"Field '{field}' cannot be updated"
        if not updatable_fields[field](value):
            return False, f"Invalid value for field '{field}'"

    return True, None