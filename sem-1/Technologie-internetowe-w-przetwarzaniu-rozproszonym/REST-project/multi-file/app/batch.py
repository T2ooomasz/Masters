from datetime import datetime
from flask import Blueprint, request, jsonify

# Importy relatywne z naszego pakietu 'app'
from .data import authors, books
from .utils import (
    create_error_response,
    generate_etag,
    idempotent_post
)

# Tworzymy instancję Blueprint dla operacji wsadowych
batch_bp = Blueprint('batch_api', __name__, url_prefix='/api/v1/batch')


@idempotent_post
@batch_bp.route('/bulk-update', methods=['POST'])
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
    not_found_ids = [book_id for book_id in book_ids if book_id not in books]
    if not_found_ids:
        return create_error_response(
            "Precondition Failed", 412,
            f"Operation failed. The following books were not found: {not_found_ids}"
        )

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
        # Poprawka błędu: updatable_fieldsfield -> updatable_fields[field](value)
        if not updatable_fields[field](value):
            return create_error_response(
                "Validation error", 400, f"Invalid value provided for field '{field}'."
            )

    # Faza 2: Wykonanie (Commit)
    updated_books = []
    for book_id in book_ids:
        books[book_id].update(updates)
        books[book_id]['updated_at'] = datetime.now().isoformat()
        books[book_id]['etag'] = generate_etag(books[book_id])
        updated_books.append(books[book_id])

    response_data = {
        "status": "Completed successfully",
        "updated_count": len(updated_books),
        "updated_books": updated_books
    }
    return jsonify(response_data), 200