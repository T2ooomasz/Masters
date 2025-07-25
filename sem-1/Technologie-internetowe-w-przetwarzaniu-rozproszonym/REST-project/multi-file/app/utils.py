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