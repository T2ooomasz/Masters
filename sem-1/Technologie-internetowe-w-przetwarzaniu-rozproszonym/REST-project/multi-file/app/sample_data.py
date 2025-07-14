import uuid
from datetime import datetime

# Importy relatywne
from .data import authors, books
from .utils import generate_etag

def initialize_sample_data():
    """Inicjalizuje przykładowe dane dla testów"""
    if authors or books: # Zapobiega ponownej inicjalizacji
        return

    sample_authors = [
        {
            "name": "J.R.R. Tolkien",
            "bio": "British author, philologist, and professor",
            "birth_year": 1892
        },
        {
            "name": "Andrzej Sapkowski",
            "bio": "Polish fantasy writer",
            "birth_year": 1948
        },
        {
            "name": "George R.R. Martin",
            "bio": "American novelist and short story writer",
            "birth_year": 1948
        }
    ]
    
    # Tworzenie autorów
    author_ids = []
    for author_data in sample_authors:
        author_id = str(uuid.uuid4())
        author = {
            "id": author_id,
            "name": author_data['name'],
            "bio": author_data['bio'],
            "birth_year": author_data['birth_year'],
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        author['etag'] = generate_etag(author)
        authors[author_id] = author
        author_ids.append(author_id)
    
    # Tworzenie przykładowych książek
    sample_books = [
        {"title": "My Random Book", "author_idx": 0, "copies": 5000, "isbn": "111-2223334445", "publication_year": 2000},
        {"title": "The Hobbit", "author_idx": 0, "copies": 5, "isbn": "978-0547928227", "publication_year": 1937},
        {"title": "The Fellowship of the Ring", "author_idx": 0, "copies": 3, "isbn": "978-0547928210", "publication_year": 1954},
        {"title": "The Two Towers", "author_idx": 0, "copies": 3, "isbn": "978-0547928203", "publication_year": 1954},
        {"title": "The Return of the King", "author_idx": 0, "copies": 3, "isbn": "978-0547928197", "publication_year": 1955},
        {"title": "The Last Wish", "author_idx": 1, "copies": 4, "isbn": "978-0316029186", "publication_year": 1993},
        {"title": "Sword of Destiny", "author_idx": 1, "copies": 4, "isbn": "978-0316389709", "publication_year": 1992},
        {"title": "Blood of Elves", "author_idx": 1, "copies": 2, "isbn": "978-0316029193", "publication_year": 1994},
        {"title": "Time of Contempt", "author_idx": 1, "copies": 2, "isbn": "978-0316219181", "publication_year": 1995},
        {"title": "A Game of Thrones", "author_idx": 2, "copies": 6, "isbn": "978-0553103540", "publication_year": 1996},
        {"title": "A Clash of Kings", "author_idx": 2, "copies": 5, "isbn": "978-0553108033", "publication_year": 1998},
        {"title": "A Storm of Swords", "author_idx": 2, "copies": 4, "isbn": "978-0553106633", "publication_year": 2000},
        {"title": "A Feast for Crows", "author_idx": 2, "copies": 3, "isbn": "978-0553801507", "publication_year": 2005},
        {"title": "A Dance with Dragons", "author_idx": 2, "copies": 3, "isbn": "978-0553801477", "publication_year": 2011},
        {"title": "Fire & Blood", "author_idx": 2, "copies": 2, "isbn": "978-1524796280", "publication_year": 2018},
        {"title": "The World of Ice & Fire", "author_idx": 2, "copies": 1, "isbn": "978-0553805444", "publication_year": 2014}
    ]
    
    for book_data in sample_books:
        book_id = str(uuid.uuid4())
        book = {
            "id": book_id,
            "title": book_data['title'],
            "author_id": author_ids[book_data['author_idx']],
            "copies": book_data['copies'],
            "isbn": book_data['isbn'],
            "publication_year": book_data['publication_year'],
            "created_at": datetime.now().isoformat(),
            "updated_at": datetime.now().isoformat()
        }
        book['etag'] = generate_etag(book)
        books[book_id] = book