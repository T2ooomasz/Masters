import uuid
from datetime import datetime
from copy import deepcopy  # Dodany dla bezpiecznych snapshotów
from flask import Blueprint, request, jsonify, url_for

# Importy relatywne z naszego pakietu 'app'
from .data import authors, books
from .utils import (
    create_error_response,
    validate_book_data,
    paginate_data,
    generate_etag,
    etag_precondition_check,
    idempotent,
    idempotent_patch
)

# Tworzymy instancję Blueprint dla książek
books_bp = Blueprint('books_api', __name__, url_prefix='/api/v1/books')

@books_bp.route('', methods=['GET', 'POST'])
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
        base_url = url_for('books_api.books_collection', _external=True)
        pagination_info = response.get_json()['pagination']
        
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
       return create_book()

@idempotent  # Wymaga Idempotency-Key
def create_book():
     # Dodawanie nowej książki
        try:
            data = request.get_json()
        except Exception:
            return create_error_response("Invalid JSON", 400)
        
        # Walidacja danych
        is_valid, error_msg = validate_book_data(data)
        if not is_valid:
            return create_error_response("Validation error", 400, error_msg)
        
        # Tworzenie nowej książki (nie potrzeba snapshota, bo nowy zasób)
        book_id = str(uuid.uuid4())
        book = {
            "id": book_id,
            "title": data['title'].strip(),
            "author_id": data['author_id'],
            "copies": data.get('copies', 1),
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        
        if 'isbn' in data: book['isbn'] = data['isbn'].strip()
        if 'publication_year' in data: book['publication_year'] = data['publication_year']
        if 'description' in data and isinstance(data['description'], str): book['description'] = data['description'].strip()
        
        book['etag'] = generate_etag(book)
        book['author_name'] = authors[data['author_id']]['name']
        
        # Faza 1: Snapshot niepotrzebny (nowy zasób), ale try-except dla atomowości zapisu
        try:
            # Faza 2: Wykonanie – zapis
            books[book_id] = book
            
            # Commit: Sukces
            response = jsonify(book)
            response.headers['Location'] = url_for('books_api.book_resource', book_id=book_id, _external=True)
            return response, 201
            
        except Exception as e:
            # Faza 3: Rollback – nie zapisano, stan niezmieniony
            return create_error_response(
                "Transaction failed",
                500,
                f"Error during book creation: {str(e)}. No changes applied."
            )

@books_bp.route('/<book_id>', methods=['GET', 'PUT', 'PATCH', 'DELETE'])
def book_resource(book_id):
    """Pojedyncza książka z obsługą ETag i Lost Update Problem"""
    if book_id not in books:
        return create_error_response("Book not found", 404)
    
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


@etag_precondition_check(resource_collection=books, required=True)
def update_book_put(book_id):
    """Obsługuje logikę dla PUT /books/{id} z wymaganym ETag – atomowo."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    is_valid, error_msg = validate_book_data(data)
    if not is_valid:
        return create_error_response("Validation error", 400, error_msg)

    current_book = deepcopy(books[book_id])  # Faza 1: Snapshot bieżącego zasobu

    try:
        # Faza 2: Wykonanie aktualizacji
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

        # Commit: Sukces
        response = jsonify(updated_book)
        response.headers['ETag'] = f'"{updated_book["etag"]}"'
        return response, 200
        
    except Exception as e:
        # Faza 3: Rollback zasobu
        books[book_id] = current_book
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during book update: {str(e)}. Book state rolled back."
        )

@etag_precondition_check(resource_collection=books, required=False)
@idempotent_patch # Wymaganie Idempotency-key
def update_book_patch(book_id):
    """Obsługuje logikę dla PATCH /books/{id} z opcjonalnym ETag – atomowo."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    if not data or not isinstance(data, dict):
        return create_error_response("Request body must be a non-empty JSON object", 400)

    current_book = deepcopy(books[book_id])  # Faza 1: Snapshot bieżącego zasobu

    try:
        # Faza 2: Walidacja i wykonanie
        validation_errors = []

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
                    if value is None: current_book.pop(field, None)
                    else: current_book[field] = value.strip() if isinstance(value, str) else value
                else: validation_errors.append(f"Invalid value for field '{field}'")

        if validation_errors: 
            raise ValueError(", ".join(validation_errors))  # Rzuć, by wejść w rollback

        current_book['updated_at'] = datetime.now().isoformat()
        current_book['etag'] = generate_etag(current_book)
        books[book_id] = current_book

        # Commit: Sukces
        response = jsonify(current_book)
        response.headers['ETag'] = f'"{current_book["etag"]}"'
        return response, 200
        
    except ValueError as ve:
        # Obsługa błędów walidacji (bez rollbacku, bo stan niezmieniony)
        return create_error_response("Validation error", 400, str(ve))
    except Exception as e:
        # Faza 3: Rollback dla innych błędów
        books[book_id] = current_book
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during book patch: {str(e)}. Book state rolled back."
        )

@etag_precondition_check(resource_collection=books, required=False)
def delete_book(book_id):
    """Obsługuje logikę dla DELETE /books/{id} z opcjonalnym ETag."""
    del books[book_id]
    return '', 204