from datetime import datetime
from copy import deepcopy  # Do snapshotu stanu
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
    Aktualizuje wiele książek w jednej operacji (atomowo).
    Oczekuje JSON: {"bookIds": ["1", "2"], "updates": {"copies": 10}, "requestId": "unique-id"}  # requestId dla idempotencji
    """
    data = request.json
    book_ids = data.get('bookIds')
    updates = data.get('updates')

    # Walidacja zbiorcza
    is_valid, error_msg = validate_batch_update_data(data)
    if not is_valid:
        return create_error_response("Validation error", 412, error_msg)

    # Sprawdź istnienie wszystkich książek (pre-walidacja)
    missing_books = [bid for bid in book_ids if bid not in books]
    if missing_books:
        return create_error_response(
            "Precondition failed: Missing books",
            412,
            f"Books not found: {missing_books}"
        )

    # Faza 1: Snapshot stanu przed transakcją (dla rollbacku)
    books_snapshot = deepcopy(books)  # Głęboka kopia dla atomowości

    try:
        # Faza 2: Wykonanie aktualizacji
        updated_books = []
        for book_id in book_ids:
            book = books[book_id]
            book.update(updates)  # Zaktualizuj pola
            book['updated_at'] = datetime.now().isoformat()
            book['etag'] = generate_etag(book)
            updated_books.append({**book})  # Kopia dla odpowiedzi, bez modyfikacji oryginału

        # Jeśli dotarliśmy tu, wszystkie update'y OK – transakcja commit (już zapisane)

        response_data = {
            "status": "Completed successfully",
            "updated_count": len(updated_books),
            "updated_book_ids": book_ids,  # Dla traceability
            "updated_books": updated_books  # Pełne dane (opcjonalnie, dla bezpieczeństwa usuń wrażliwe pola)
        }
        return jsonify(response_data), 200

    except Exception as e:
        # Faza 3: Rollback w razie błędu
        books.clear()
        books.update(books_snapshot)  # Przywróć stan
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during bulk update: {str(e)}. State rolled back."
        )