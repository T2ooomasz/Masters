import uuid
from datetime import datetime
from copy import deepcopy  # Dla bezpiecznych snapshotów
from flask import Blueprint, request, jsonify, url_for

# Importy relatywne z naszego pakietu 'app'
from .data import authors, books  # Books dla kaskadowego usuwania
from .utils import (
    create_error_response,
    validate_author_data,
    paginate_data,
    generate_etag,
    etag_precondition_check,
    idempotent,
    idempotent_patch
)

# 1. Tworzymy instancję Blueprint.
# 'authors_api' to nazwa blueprintu, __name__ pomaga Flaskowi zlokalizować zasoby.
# url_prefix automatycznie dodaje '/api/v1/authors' do wszystkich tras w tym pliku.
authors_bp = Blueprint('authors_api', __name__, url_prefix='/api/v1/authors')

# 2. Przenosimy trasę dla kolekcji autorów.
# Zwróć uwagę, że dekorator to teraz @authors_bp.route, a ścieżka to ''
# ponieważ prefix jest już zdefiniowany w blueprincie.
@authors_bp.route('', methods=['GET', 'POST'])
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
        # Ważna zmiana: url_for używa teraz 'nazwa_blueprintu.nazwa_funkcji'
        base_url = url_for('authors_api.authors_collection', _external=True)
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
        # Wywołaj idempotentną funkcję – dekorator obsłuży klucz i cache
        return create_author()

# Nowa funkcja dla POST – idempotentna
@idempotent  # Twój dekorator z utils.py – wymaga Idempotency-Key
def create_author():
    """Tworzy nowego autora (idempotentne via Idempotency-Key)."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)
    
    is_valid, error_msg = validate_author_data(data)
    if not is_valid:
        return create_error_response("Validation error", 400, error_msg)
    
    author_id = str(uuid.uuid4())
    author = {
        "id": author_id, 
        "name": data['name'].strip(), 
        "created_at": datetime.now().isoformat(), 
        "updated_at": datetime.now().isoformat()
    }
    if 'bio' in data and isinstance(data['bio'], str):
        author['bio'] = data['bio'].strip()
    if 'birth_year' in data and isinstance(data['birth_year'], int):
        author['birth_year'] = data['birth_year']
    author['etag'] = generate_etag(author)
    
    # Faza 1: Snapshot niepotrzebny (nowy zasób), ale try-except dla atomowości zapisu
    try:
        # Faza 2: Wykonanie – zapis
        authors[author_id] = author
        
        # Commit: Sukces
        response = jsonify(author)
        response.headers['Location'] = url_for('authors_api.author_resource', author_id=author_id, _external=True)
        return response, 201
        
    except Exception as e:
        # Faza 3: Rollback – nie zapisano, stan niezmieniony
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during author creation: {str(e)}. No changes applied."
        )

@authors_bp.route('/<author_id>', methods=['GET', 'PUT', 'PATCH', 'DELETE'])
def author_resource(author_id):
    """
    GET /authors/{id} - Zwraca informacje o autorze
    PUT /authors/{id} - Aktualizuje autora (pełna aktualizacja)
    PATCH /authors/{id} - Aktualizuje autora (częściowa aktualizacja)
    DELETE /authors/{id} - Usuwa autora
    """
    if author_id not in authors:
        return create_error_response("Author not found", 404, 
                                   f"Author with id '{author_id}' does not exist")
    
    if request.method == 'GET':
        author = authors[author_id]
        response = jsonify(author)
        response.headers['ETag'] = f'"{author["etag"]}"'
        return response, 200
    
    elif request.method == 'PUT':
        return update_author_put(author_id)
    
    elif request.method == 'PATCH':
        return update_author_patch(author_id)
    
    elif request.method == 'DELETE':
        return delete_author(author_id)


@etag_precondition_check(resource_collection=authors, required=True)
def update_author_put(author_id):
    """Obsługuje logikę dla PUT /authors/{id} z wymaganym ETag – atomowo."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    is_valid, error_msg = validate_author_data(data)
    if not is_valid:
        return create_error_response("Validation error", 400, error_msg)

    current_author = deepcopy(authors[author_id])  # Faza 1: Snapshot bieżącego zasobu

    try:
        # Faza 2: Wykonanie aktualizacji
        updated_author = {
            "id": author_id,
            "name": data['name'].strip(),
            "created_at": current_author['created_at'],
            "updated_at": datetime.now().isoformat()
        }
        if 'bio' in data:
            updated_author['bio'] = data['bio'].strip() if isinstance(data['bio'], str) else ''
        if 'birth_year' in data:
            updated_author['birth_year'] = data['birth_year'] if isinstance(data['birth_year'], int) else None

        updated_author['etag'] = generate_etag(updated_author)
        authors[author_id] = updated_author

        # Commit: Sukces
        response = jsonify(updated_author)
        response.headers['ETag'] = f'"{updated_author["etag"]}"'
        return response, 200
        
    except Exception as e:
        # Faza 3: Rollback zasobu
        authors[author_id] = current_author
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during author update: {str(e)}. Author state rolled back."
        )

@etag_precondition_check(resource_collection=authors, required=False)
@idempotent_patch # Wymaganie Idempotency-key
def update_author_patch(author_id):
    """Obsługuje logikę dla PATCH /authors/{id} z opcjonalnym ETag – atomowo."""
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    if not data or not isinstance(data, dict):
        return create_error_response("Request body must be a non-empty JSON object", 400)

    current_author = deepcopy(authors[author_id])  # Faza 1: Snapshot bieżącego zasobu

    try:
        # Faza 2: Walidacja i wykonanie
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
            raise ValueError(", ".join(validation_errors))  # Rzuć, by wejść w except

        # Dodatkowa walidacja: name nie może być puste
        if 'name' not in current_author or not current_author.get('name'):
            raise ValueError("Field 'name' cannot be removed or empty")

        current_author['updated_at'] = datetime.now().isoformat()
        current_author['etag'] = generate_etag(current_author)
        authors[author_id] = current_author

        # Commit: Sukces
        response = jsonify(current_author)
        response.headers['ETag'] = f'"{current_author["etag"]}"'
        return response, 200
        
    except ValueError as ve:
        # Obsługa błędów walidacji (bez rollbacku, bo stan niezmieniony)
        return create_error_response("Validation error", 400, str(ve))
    except Exception as e:
        # Faza 3: Rollback dla innych błędów
        authors[author_id] = current_author
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during author patch: {str(e)}. Author state rolled back."
        )

@etag_precondition_check(resource_collection=authors, required=False)
def delete_author(author_id):
    """Obsługuje logikę dla DELETE /authors/{id} z opcjonalnym ETag – atomowo z kaskadowym usuwaniem książek."""
    if author_id not in authors:
        return create_error_response("Author not found", 404, f"Author {author_id} does not exist")
    
    # Faza 1: Znajdź i snapshot dotknięte zasoby
    current_author = deepcopy(authors[author_id])  # Snapshot autora
    
    # Znajdź książki tego autora
    books_to_delete = {book_id: deepcopy(book) for book_id, book in books.items() if book['author_id'] == author_id}
    if not books_to_delete:
        # Jeśli brak książek, po prostu usuń autora
        try:
            del authors[author_id]
            return '', 204
        except Exception as e:
            authors[author_id] = current_author
            return create_error_response(
                "Transaction failed",
                500,
                f"Error during author deletion: {str(e)}. Author state rolled back."
            )
    
    try:
        # Faza 2: Wykonanie atomowe
        # Usuń książki
        for book_id in books_to_delete:
            del books[book_id]
        
        # Usuń autora
        del authors[author_id]
        
        # Commit: Sukces (DELETE zwraca 204, bez body)
        return '', 204
        
    except Exception as e:
        # Faza 3: Rollback – przywróć autora i książki
        authors[author_id] = current_author
        for book_id, snapshot in books_to_delete.items():
            books[book_id] = snapshot
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during author deletion: {str(e)}. Author and books state rolled back."
        )