# Import the application factory
from app import create_app
# Import the data initialization function
from app.sample_data import initialize_sample_data
# Import data for the startup message
from app.data import authors, books

# Initialize the sample data
initialize_sample_data()

# Create the Flask app instance using the factory
app = create_app()

# This block runs only when the script is executed directly
if __name__ == '__main__':
    
    # Startup message
    startup_message = """=== Library Management System - Authors, Books, Orders, Bulk Update ===
Dostępne endpointy (zgodnie z REST):

[AUTHORS]
  GET    /api/v1/authors             - Lista autorów
  POST   /api/v1/authors             - Dodaj autora
  GET    /api/v1/authors/{{id}}        - Info o autorze
  PUT    /api/v1/authors/{{id}}        - Aktualizacja autora (pełna)
  PATCH  /api/v1/authors/{{id}}        - Aktualizacja autora (częściowa)
  DELETE /api/v1/authors/{{id}}        - Usuń autora

[BOOKS]
  GET    /api/v1/books               - Lista książek (stronicowanie)
  POST   /api/v1/books               - Dodaj książkę
  GET    /api/v1/books/{{id}}          - Info o książce
  PUT    /api/v1/books/{{id}}          - Aktualizacja książki (ETag)
  PATCH  /api/v1/books/{{id}}          - Częściowa aktualizacja książki (ETag)
  DELETE /api/v1/books/{{id}}          - Usuń książkę

[ORDERS]
  GET    /api/v1/orders              - Lista zleceń
  GET    /api/v1/orders/{{id}}         - Info o zleceniu
  POST   /api/v1/orders              - Utwórz zlecenie (idempotentne)

[BULK UPDATE]
  POST   /api/v1/batch/bulk-update   - Zbiorcza aktualizacja książek

Dane przykładowe: {} autorów, {} książek

Serwer uruchomiony na http://localhost:5000
=================================================================
""".format(len(authors), len(books))
    
    print(startup_message.encode('utf-8'))
    # Run the development server
    app.run(debug=True, host='0.0.0.0', port=5000)
    print(app.url_map)