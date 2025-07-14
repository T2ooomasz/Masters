import uuid
from datetime import datetime
from flask import Blueprint, request, jsonify, url_for

# Importy relatywne z naszego pakietu 'app'
from .data import authors
from .utils import (
    create_error_response,
    validate_author_data,
    paginate_data,
    generate_etag,
    etag_precondition_check
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
        try: data = request.get_json()
        except Exception: return create_error_response("Invalid JSON", 400)
        is_valid, error_msg = validate_author_data(data)
        if not is_valid: return create_error_response("Validation error", 400, error_msg)
        
        author_id = str(uuid.uuid4())
        author = {"id": author_id, "name": data['name'].strip(), "created_at": datetime.now().isoformat(), "updated_at": datetime.now().isoformat()}
        if 'bio' in data and isinstance(data['bio'], str): author['bio'] = data['bio'].strip()
        if 'birth_year' in data and isinstance(data['birth_year'], int): author['birth_year'] = data['birth_year']
        author['etag'] = generate_etag(author)
        authors[author_id] = author
        
        response = jsonify(author)
        response.headers['Location'] = url_for('authors_api.author_resource', author_id=author_id, _external=True)
        return response, 201

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
    """Obsługuje logikę dla PUT /authors/{id} z wymaganym ETag."""
    try: data = request.get_json()
    except Exception: return create_error_response("Invalid JSON", 400)
    is_valid, error_msg = validate_author_data(data)
    if not is_valid: return create_error_response("Validation error", 400, error_msg)
    current_author = authors[author_id]
    updated_author = {"id": author_id, "name": data['name'].strip(), "created_at": current_author['created_at'], "updated_at": datetime.now().isoformat()}
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
    try: data = request.get_json()
    except Exception: return create_error_response("Invalid JSON", 400)
    if not data or not isinstance(data, dict): return create_error_response("Request body must be a non-empty JSON object", 400)
    current_author = authors[author_id].copy()
    validation_errors = []
    updatable_fields = {'name': lambda v: isinstance(v, str) and v.strip(), 'bio': lambda v: isinstance(v, str) or v is None, 'birth_year': lambda v: isinstance(v, int) or v is None}
    for field, value in data.items():
        if field in updatable_fields:
            if updatable_fields[field](value):
                if value is None: current_author.pop(field, None)
                else: current_author[field] = value.strip() if isinstance(value, str) else value
            else: validation_errors.append(f"Invalid value for field '{field}'")
    if validation_errors: return create_error_response("Validation error", 400, ", ".join(validation_errors))
    if 'name' not in current_author or not current_author.get('name'): return create_error_response("Validation error", 400, "Field 'name' cannot be removed or empty")
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