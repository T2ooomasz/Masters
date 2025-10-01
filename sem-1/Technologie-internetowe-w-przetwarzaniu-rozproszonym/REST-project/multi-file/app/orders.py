import uuid
from datetime import datetime
from copy import deepcopy  # Dla snapshotu pojedynczych zasobów
from flask import Blueprint, request, jsonify, url_for

# Importy relatywne z naszego pakietu 'app'
from .data import books, orders
from .utils import (
    create_error_response,
    paginate_data,
    idempotent,
    generate_etag,
    etag_precondition_check,
    idempotent_patch  # Dla PATCH zwrotów
)

# Tworzymy instancję Blueprint dla zamówień
orders_bp = Blueprint('orders_api', __name__, url_prefix='/api/v1/orders')


@idempotent
def create_order():
    """Tworzy nowe zlecenie atomowo (order + update book)."""
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

    # Sprawdzanie dostępności
    if books[book_id]['copies'] < quantity:
        return create_error_response("Insufficient stock", 409, 
                                   f"Not enough copies of '{books[book_id]['title']}'. Available: {books[book_id]['copies']}")

    # Faza 1: Snapshot TYLKO dotkniętego booka
    book_snapshot = deepcopy(books[book_id])

    try:
        # Faza 2: Wykonanie
        order_id = str(uuid.uuid4())
        order = {
            **data,
            "id": order_id,
            "status": "created",
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        order['etag'] = generate_etag(order)

        # Odejmij copies
        books[book_id]['copies'] -= quantity
        books[book_id]['updated_at'] = datetime.now().isoformat()
        books[book_id]['etag'] = generate_etag(books[book_id])

        # Zapisz order
        orders[order_id] = order

        response = jsonify(order)
        response.headers['Location'] = url_for('orders_api.order_resource', order_id=order_id, _external=True)
        return response, 201

    except Exception as e:
        # Faza 3: Rollback TYLKO booka
        books[book_id] = book_snapshot
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during order creation: {str(e)}. Book state rolled back."
        )


@idempotent
def bulk_create_orders():
    """
    Atomowo tworzy wiele zamówień (bulk order), aktualizując orders i books.
    Oczekuje JSON: {"items": [{"bookId": "uuid", "quantity": 2}, ...]}
    """
    try:
        data = request.get_json()
    except Exception:
        return create_error_response("Invalid JSON", 400)

    if not data or not isinstance(data, dict):
        return create_error_response("Invalid JSON payload", 400)

    items = data.get('items', [])
    if not items or not isinstance(items, list) or len(items) > 50:
        return create_error_response("Validation error", 400, "items must be a non-empty list with max 50 elements")

    # Faza 1: Walidacja zbiorcza
    validation_errors = []
    book_quantities = {}  # book_id -> total quantity (na wypadek duplikatów books)
    for idx, item in enumerate(items):
        book_id = item.get('bookId')
        quantity = item.get('quantity')
        if not book_id or book_id not in books:
            validation_errors.append(f"Item {idx}: Book '{book_id}' not found")
            continue
        if not isinstance(quantity, int) or quantity <= 0:
            validation_errors.append(f"Item {idx}: quantity must be positive integer")
            continue
        book_quantities[book_id] = book_quantities.get(book_id, 0) + quantity
        if books[book_id]['copies'] < book_quantities[book_id]:
            validation_errors.append(f"Item {idx}: Insufficient copies for '{books[book_id]['title']}' (need {book_quantities[book_id]}, available {books[book_id]['copies']})")

    if validation_errors:
        return create_error_response("Validation failed", 400, "; ".join(validation_errors[:10]))  # Limit błędów

    # Faza 2: Snapshot tylko dotkniętych books
    book_snapshots = {book_id: deepcopy(books[book_id]) for book_id in book_quantities}

    try:
        # Faza 3: Wykonanie atomowe
        created_orders = []
        for item in items:
            book_id = item['bookId']
            quantity = item['quantity']
            order_id = str(uuid.uuid4())
            order = {
                **item,  # bookId i quantity
                "id": order_id,
                "status": "created",
                "created_at": datetime.now().isoformat(),
                "updated_at": datetime.now().isoformat()
            }
            order['etag'] = generate_etag(order)

            # Odejmij quantity (uwzględniając duplikaty via book_quantities, ale tu per-item)
            books[book_id]['copies'] -= quantity
            books[book_id]['updated_at'] = datetime.now().isoformat()
            books[book_id]['etag'] = generate_etag(books[book_id])

            orders[order_id] = order
            created_orders.append(order_id)

        # Commit: Zmiany zapisane
        response_data = {
            "status": "Bulk order created successfully",
            "created_count": len(created_orders),
            "order_ids": created_orders
        }
        response = jsonify(response_data)
        response.headers['Location'] = url_for('orders_api.orders_collection', _external=True)  # Link do kolekcji
        return response, 201

    except Exception as e:
        # Faza 4: Rollback tylko dotkniętych books
        for book_id, snapshot in book_snapshots.items():
            books[book_id] = snapshot
        return create_error_response(
            "Bulk transaction failed",
            500,
            f"Error during bulk order creation: {str(e)}. Books state rolled back."
        )


@orders_bp.route('', methods=['GET', 'POST'])
def orders_collection():
    """Zlecenia z obsługą idempotencji"""
    if request.method == 'GET':
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

        links = []
        base_url = url_for('orders_api.orders_collection', _external=True)
        pagination_info = response.get_json()['pagination']

        if pagination_info['has_next']: links.append(f'<{base_url}?page={page + 1}&limit={limit}>; rel="next"')
        if pagination_info['has_previous']: links.append(f'<{base_url}?page={page - 1}&limit={limit}>; rel="prev"')
        if links: response.headers['Link'] = ', '.join(links)
        return response, 200
    
    else:  # POST
        return create_order()


@orders_bp.route('/bulk', methods=['POST'])
def bulk_orders_endpoint():
    """Endpoint dla bulk POST - wywołuje wyodrębnioną funkcję z dekoratorem."""
    return bulk_create_orders()


@orders_bp.route('/<order_id>', methods=['GET', 'DELETE'])  # Dodano 'DELETE'
def order_resource(order_id):
    """Zwraca informacje o pojedynczym zleceniu lub usuwa je"""
    if order_id not in orders:
        return create_error_response("Order not found", 404,
                                     f"Order with id '{order_id}' does not exist")
    
    if request.method == 'GET':
        order = orders[order_id]
        response = jsonify(order)
        response.headers['ETag'] = f'"{order["etag"]}"'
        return response, 200
    
    else:  # DELETE
        return delete_order(order_id)


@etag_precondition_check(resource_collection=orders, required=False)  # Opcjonalny ETag dla DELETE
@etag_precondition_check(resource_collection=orders, required=False)  # Opcjonalny ETag dla DELETE
def delete_order(order_id):
    """Atomowo usuwa order i przywraca copies w booku (tylko jeśli status 'created')."""
    if order_id not in orders:
        return create_error_response("Order not found", 404, f"Order {order_id} does not exist")
    
    order = orders[order_id]
    if order['status'] != 'created':
        return create_error_response("Cannot delete", 409, f"Order {order_id} is not in 'created' status")
    
    book_id = order.get('bookId')
    quantity = order.get('quantity', 0)
    if not book_id or quantity <= 0:
        return create_error_response("Invalid order data", 400, "bookId and quantity > 0 required")
    
    if book_id not in books:
        return create_error_response("Book not found", 404, f"Book {book_id} does not exist")
    
    book = books[book_id]
    
    # Faza 1: Snapshot ordera i booka
    order_snapshot = deepcopy(order)
    book_snapshot = deepcopy(book)
    
    try:
        # Faza 2: Wykonanie atomowe
        # Usuń order
        del orders[order_id]
        
        # Przywróć copies w booku
        book['copies'] += quantity
        book['updated_at'] = datetime.now().isoformat()
        book['etag'] = generate_etag(book)
        
        # Commit: Sukces
        return '', 204
        
    except Exception as e:
        # Faza 3: Rollback – przywróć order i book
        orders[order_id] = order_snapshot
        books[book_id] = book_snapshot
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during order deletion: {str(e)}. State rolled back."
        )


@orders_bp.route('/<order_id>/return', methods=['PATCH'])  # Nowy endpoint dla zwrotu
@idempotent_patch  # Idempotentny PATCH
@etag_precondition_check(resource_collection=orders, required=True)  # Wymagany ETag dla bezpieczeństwa
def return_order(order_id):
    """Atomowo symuluje zwrot: zmienia status ordera na 'returned', zwiększa copies w booku."""
    if order_id not in orders:
        return create_error_response("Order not found", 404, f"Order {order_id} does not exist")
    
    order = orders[order_id]
    if order['status'] != 'created':  # Zakładam, że zwrot tylko dla 'created' (lub dostosuj)
        return create_error_response("Cannot return", 409, f"Order {order_id} is not in 'created' status")
    
    book_id = order.get('bookId')
    quantity = order.get('quantity', 0)
    if not book_id or quantity <= 0:
        return create_error_response("Invalid order data", 400, "bookId and quantity > 0 required")
    
    if book_id not in books:
        return create_error_response("Book not found", 404, f"Book {book_id} does not exist")
    
    book = books[book_id]
    
    # Faza 1: Snapshot booka
    book_snapshot = deepcopy(book)
    
    try:
        # Faza 2: Wykonanie atomowe
        # Update order
        order['status'] = 'returned'
        order['returned_at'] = datetime.now().isoformat()
        order['updated_at'] = datetime.now().isoformat()
        order['etag'] = generate_etag(order)
        
        # Update book: zwiększ copies
        book['copies'] += quantity
        book['updated_at'] = datetime.now().isoformat()
        book['etag'] = generate_etag(book)
        
        # Commit: Zmiany zapisane
        response_data = {
            "status": "Order returned successfully",
            "order_id": order_id,
            "updated_resources": {
                "order": {"new_status": "returned"},
                "book": {"id": book_id, "new_copies": book['copies']}
            }
        }
        response = jsonify(response_data)
        response.headers['ETag'] = f'"{order["etag"]}"'
        return response, 200
        
    except Exception as e:
        # Faza 3: Rollback booka
        books[book_id] = book_snapshot
        return create_error_response(
            "Transaction failed",
            500,
            f"Error during order return: {str(e)}. Book state rolled back."
        )


@orders_bp.route('/bulk-returned', methods=['DELETE'])  # Nowy bulk DELETE
def bulk_delete_returned_orders():
    """Bulk usuwa wszystkie orders ze statusem 'returned'."""
    # Zbierz orders do usunięcia
    to_delete = [oid for oid, o in orders.items() if o['status'] == 'returned']
    if not to_delete:
        return '', 204  # Nic do usunięcia
    
    # Faza 1: Snapshot usuniętych orders (dla rollbacku)
    delete_snapshot = {oid: deepcopy(orders[oid]) for oid in to_delete}
    
    try:
        # Faza 2: Wykonanie – usuń
        for order_id in to_delete:
            del orders[order_id]
        
        # Commit: Sukces
        response_data = {
            "status": "Bulk delete completed",
            "deleted_count": len(to_delete)
        }
        return jsonify(response_data), 200
        
    except Exception as e:
        # Faza 3: Rollback – przywróć usunięte
        for order_id, snapshot in delete_snapshot.items():
            orders[order_id] = snapshot
        return create_error_response(
            "Bulk delete failed",
            500,
            f"Error during bulk delete: {str(e)}. Orders restored."
        )