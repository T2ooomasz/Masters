import uuid
from datetime import datetime
from flask import Blueprint, request, jsonify, url_for

# Importy relatywne z naszego pakietu 'app'
from .data import books, orders
from .utils import (
    create_error_response,
    paginate_data,
    idempotent_post
)

# Tworzymy instancję Blueprint dla zamówień
orders_bp = Blueprint('orders_api', __name__, url_prefix='/api/v1/orders')


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
    response.headers['Location'] = url_for('orders_api.order_resource', order_id=order_id, _external=True)
    return response, 201


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
        pagination_info = result['pagination']

        if pagination_info['has_next']: links.append(f'<{base_url}?page={page + 1}&limit={limit}>; rel="next"')
        if pagination_info['has_previous']: links.append(f'<{base_url}?page={page - 1}&limit={limit}>; rel="prev"')
        if links: response.headers['Link'] = ', '.join(links)
        return response, 200
    
    else:  # POST
        return create_order()


@orders_bp.route('/<order_id>', methods=['GET'])
def order_resource(order_id):
    """Zwraca informacje o pojedynczym zleceniu"""
    if order_id not in orders:
        return create_error_response("Order not found", 404,
                                     f"Order with id '{order_id}' does not exist")
    
    order = orders[order_id]
    response = jsonify(order)
    return response, 200