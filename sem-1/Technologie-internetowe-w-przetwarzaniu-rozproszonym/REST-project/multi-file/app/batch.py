from datetime import datetime
from flask import Blueprint, request, jsonify

# Importy relatywne z naszego pakietu 'app'
from .data import authors, books
from .utils import (
    create_error_response,
    generate_etag,
    idempotent,
    validate_batch_update_data
)

# Tworzymy instancję Blueprint dla operacji wsadowych
batch_bp = Blueprint('batch_api', __name__, url_prefix='/api/v1/batch')


@idempotent
@batch_bp.route('/bulk-update', methods=['POST'])
def bulk_update():
    """
    Aktualizuje wiele książek w jednej operacji.
    Oczekuje JSON: {"bookIds": ["1", "2"], "updates": {"copies": 10}}
    """
    data = request.json
    book_ids = data.get('bookIds')
    updates = data.get('updates')

    # Używamy nowej funkcji walidującej
    is_valid, error_msg = validate_batch_update_data(data)
    if not is_valid:
        return create_error_response("Validation error", 412, error_msg)

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
