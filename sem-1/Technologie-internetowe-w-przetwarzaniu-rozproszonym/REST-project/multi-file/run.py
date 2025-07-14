# Import the application factory
from app import create_app
# Import the data initialization function
from app.sample_data import initialize_sample_data
# Import data for the startup message
from app.data import authors, books

# Create the Flask app instance using the factory
app = create_app()

# This block runs only when the script is executed directly
if __name__ == '__main__':
    # The app context is required for operations like url_for,
    # and it's good practice to initialize data within it.
    with app.app_context():
        initialize_sample_data()
    
    # Startup message
    print("=== Library Management System - Authors, Books, Orders, Bulk Update ===")
    print("Dostępne endpointy (zgodnie z REST):")
    print("\n[AUTHORS]")
    print("  GET    /api/v1/authors             - Lista autorów")
    print("  POST   /api/v1/authors             - Dodaj autora")
    print("  GET    /api/v1/authors/{id}        - Info o autorze")
    print("  PUT    /api/v1/authors/{id}        - Aktualizacja autora (pełna)")
    print("  PATCH  /api/v1/authors/{id}        - Aktualizacja autora (częściowa)")
    print("  DELETE /api/v1/authors/{id}        - Usuń autora")

    print("\n[BOOKS]")
    print("  GET    /api/v1/books               - Lista książek (stronicowanie)")
    print("  POST   /api/v1/books               - Dodaj książkę")
    print("  GET    /api/v1/books/{id}          - Info o książce")
    print("  PUT    /api/v1/books/{id}          - Aktualizacja książki (ETag)")
    print("  PATCH  /api/v1/books/{id}          - Częściowa aktualizacja książki (ETag)")
    print("  DELETE /api/v1/books/{id}          - Usuń książkę")

    print("\n[ORDERS]")
    print("  GET    /api/v1/orders              - Lista zleceń")
    print("  GET    /api/v1/orders/{id}         - Info o zleceniu")
    print("  POST   /api/v1/orders              - Utwórz zlecenie (idempotentne)")

    print("\n[BULK UPDATE]")
    print("  POST   /api/v1/batch/bulk-update   - Zbiorcza aktualizacja książek")

    print(f"\nDane przykładowe: {len(authors)} autorów, {len(books)} książek")
    print("\nSerwer uruchomiony na http://localhost:5000")
    print("=" * 65)
    
    # Run the development server
    app.run(debug=True, host='0.0.0.0', port=5000)