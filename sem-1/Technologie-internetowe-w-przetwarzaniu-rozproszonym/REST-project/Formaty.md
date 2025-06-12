Znaczenie Operacji HTTP i Formaty Danych
Poniżej znajduje się opis każdego zasobu, znaczenia operacji HTTP, które można na nim wykonać, oraz formatów danych wejściowych i wyjściowych.

Zasób: /authors
Reprezentuje kolekcję wszystkich autorów w systemie.

GET /api/v1/authors
Znaczenie: Pobiera listę wszystkich autorów.
Dane wejściowe: Brak.
Format wyjściowy: Obiekt JSON zawierający listę autorów oraz ich łączną liczbę.
JSON
'''
{
  "data": [
    {
      "id": "c3d4e5f6-a1b2-...",
      "name": "J.R.R. Tolkien",
      "bio": "British author, philologist, and professor",
      "birth_year": 1892,
      "created_at": "2025-06-12T13:30:00.123456",
      "updated_at": "2025-06-12T13:30:00.123456",
      "etag": "a1b2c3d4e5f6a7b8"
    }
  ],
  "total": 1
}
'''
POST /api/v1/authors
Znaczenie: Tworzy nowego autora w systemie.
Dane wejściowe: Obiekt JSON z danymi autora. Pole name jest wymagane. Pola bio i birth_year są opcjonalne.
JSON
'''
{
  "name": "George Orwell",
  "bio": "English novelist, essayist, journalist and critic.",
  "birth_year": 1903
}
'''
Format wyjściowy: Pełny obiekt nowo utworzonego autora, zawierający pola wygenerowane przez serwer (id, created_at, updated_at, etag).
Zasób: /authors/{id}
Reprezentuje pojedynczego autora, identyfikowanego przez jego unikalny identyfikator.

GET /api/v1/authors/{id}
Znaczenie: Pobiera szczegółowe informacje o jednym autorze.
Dane wejściowe: author_id w ścieżce URL.
Format wyjściowy: Obiekt JSON z danymi autora. Odpowiedź zawiera również nagłówek ETag z aktualną wersją zasobu.
PUT /api/v1/authors/{id}
Znaczenie: Pełna aktualizacja (zamiana) istniejącego autora. Wszystkie wymagane pola muszą być dostarczone.
Dane wejściowe: author_id w ścieżce URL oraz obiekt JSON z kompletnymi, nowymi danymi autora.
Format wyjściowy: Zaktualizowany obiekt autora.
PATCH /api/v1/authors/{id}
Znaczenie: Częściowa aktualizacja istniejącego autora. Zmienia tylko te pola, które zostały dostarczone w ciele żądania.
Dane wejściowe: author_id w ścieżce URL oraz obiekt JSON z polami do zmiany.
Format wyjściowy: Zaktualizowany obiekt autora.
DELETE /api/v1/authors/{id}
Znaczenie: Usuwa autora z systemu.
Dane wejściowe: author_id w ścieżce URL.
Format wyjściowy: Brak ciała odpowiedzi (status 204 No Content).
Zasób: /books
Reprezentuje kolekcję wszystkich książek w systemie.

GET /api/v1/books
Znaczenie: Pobiera listę książek z obsługą stronicowania.
Dane wejściowe: Opcjonalne parametry zapytania ?page= (numer strony) i ?limit= (liczba wyników na stronę).
Format wyjściowy: Obiekt JSON zawierający listę książek (data) oraz metadane stronicowania (pagination).
JSON
'''
{
  "data": [
    {
      "id": "1",
      "title": "The Hobbit",
      "author_id": "c3d4e5f6-a1b2-...",
      "author_name": "J.R.R. Tolkien",
      "copies": 5,
      "etag": "b2c3d4e5f6a7b8c9",
      "..."
    }
  ],
  "pagination": {
    "current_page": 1,
    "per_page": 10,
    "total_items": 16,
    "total_pages": 2,
    "has_next": true,
    "has_previous": false
  }
}
'''
POST /api/v1/books
Znaczenie: Tworzy nową książkę w systemie. Wymaga istnienia autora o podanym author_id.
Dane wejściowe: Obiekt JSON z danymi książki. Wymagane pola to title i author_id. Opcjonalne to m.in. copies, isbn, publication_year.
Format wyjściowy: Pełny obiekt nowo utworzonej książki.
Zasób: /books/{id}
Reprezentuje pojedynczą książkę, chronioną przed problemem "lost update" za pomocą ETag.

GET /api/v1/books/{id}
Znaczenie: Pobiera szczegółowe informacje o jednej książce.
Dane wejściowe: book_id w ścieżce URL.
Format wyjściowy: Obiekt JSON z danymi książki oraz nagłówek ETag z jej aktualną wersją.
PUT /api/v1/books/{id}
Znaczenie: Pełna, warunkowa aktualizacja książki. Zapobiega nadpisaniu zmian, jeśli zasób został w międzyczasie zmodyfikowany przez innego klienta.
Dane wejściowe: book_id w ścieżce URL, nagłówek If-Match z aktualną wartością ETag oraz obiekt JSON z kompletnymi danymi książki.
Format wyjściowy: Zaktualizowany obiekt książki. W przypadku niezgodności ETag zwracany jest błąd 412 Precondition Failed.
PATCH /api/v1/books/{id}
Znaczenie: Częściowa, warunkowa aktualizacja książki.
Dane wejściowe: book_id w ścieżce URL, opcjonalny nagłówek If-Match oraz obiekt JSON z polami do zmiany.
Format wyjściowy: Zaktualizowany obiekt książki.
DELETE /api/v1/books/{id}
Znaczenie: Warunkowe usunięcie książki.
Dane wejściowe: book_id w ścieżce URL, opcjonalny nagłówek If-Match.
Format wyjściowy: Brak ciała odpowiedzi (status 204 No Content).
Zasób: /orders
Reprezentuje kolekcję zleceń, z mechanizmem eliminującym duplikaty.

GET /api/v1/orders
Znaczenie: Pobiera listę wszystkich złożonych zleceń.
Dane wejściowe: Brak.
Format wyjściowy: Obiekt JSON z listą wszystkich obiektów zleceń.
POST /api/v1/orders
Znaczenie: Tworzy nowe zlecenie w sposób idempotentny ("exactly once"). Powtórne wysłanie tego samego żądania z tym samym kluczem nie spowoduje utworzenia nowego zlecenia.
Dane wejściowe: Obiekt JSON z danymi zlecenia oraz nagłówek Idempotency-Key z unikalną wartością identyfikującą żądanie.
JSON

// Nagłówek: Idempotency-Key: "unikalny-klucz-12345"
// Ciało:
'''
{
  "bookId": "1",
  "quantity": 2
}
'''
Format wyjściowy: Obiekt nowo utworzonego zlecenia. W przypadku duplikatu zwracana jest pierwotna odpowiedź.
Zasób: /bulk-update
Zasób typu kontroler, umożliwiający wykonanie operacji na wielu innych zasobach atomowo.

POST /api/v1/bulk-update
Znaczenie: Zbiorcza aktualizacja wielu książek w ramach jednej operacji.
Dane wejściowe: Obiekt JSON zawierający listę identyfikatorów książek (bookIds) oraz słownik ze zmianami do wprowadzenia (updates).
JSON
'''
{
  "bookIds": ["1", "2", "999"],
  "updates": {
    "copies": 0,
    "status": "out_of_stock"
  }
}
'''
Format wyjściowy: Obiekt JSON z podsumowaniem operacji, zawierający listę zaktualizowanych książek (updated) oraz listę ID, których nie znaleziono (notFound).