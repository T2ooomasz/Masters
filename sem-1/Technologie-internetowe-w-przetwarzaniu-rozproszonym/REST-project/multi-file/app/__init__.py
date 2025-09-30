from flask import Flask, request

# Import the blueprints from their respective modules
from .authors import authors_bp
from .books import books_bp
from .orders import orders_bp
from .batch import batch_bp

# Import the error response utility
from .utils import create_error_response

def create_app():
    """
    Application factory function. This function is responsible for
    creating and configuring the Flask application instance.
    """
    app = Flask(__name__)

    # Register all the blueprints with the app instance
    app.register_blueprint(authors_bp)
    app.register_blueprint(books_bp)
    app.register_blueprint(orders_bp)
    app.register_blueprint(batch_bp)

    # Register global hooks and error handlers
    @app.after_request
    def add_self_link_header(response):
        """Dodaje nagłówek Link: rel="self" do każdej udanej odpowiedzi JSON."""
        if response.status_code >= 200 and response.status_code < 300 and \
           response.mimetype == 'application/json':
            self_link = f'<{request.url}>; rel="self"'
            if 'Link' in response.headers:
                response.headers['Link'] = f"{response.headers['Link']}, {self_link}"
            else:
                response.headers['Link'] = self_link
        return response

    @app.errorhandler(404)
    def not_found(error):
        return create_error_response("Not Found", 404, "The requested resource was not found")

    @app.errorhandler(405)
    def method_not_allowed(error):
        return create_error_response("Method Not Allowed", 405, "The method is not allowed for the requested URL")

    @app.errorhandler(500)
    def internal_server_error(error):
        return create_error_response("Internal Server Error", 500, "An unexpected error occurred")
    
    @app.route('/test')
    def test():
        return "Server is up!", 200

    return app