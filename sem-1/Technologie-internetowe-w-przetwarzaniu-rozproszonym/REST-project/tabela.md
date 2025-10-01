# Operacje HTTP na Zasobach
| URI | GET | POST | PUT | PATCH | DELETE |
|-----|-----|------|-----|-------|--------|
| /api/v1/authors | lista autorów (stronicowana) | dodaj autora (idempotentne) | X | X | X |
| /api/v1/authors/{id} | info o autorze (ETag) | X | aktualizacja (ETag) | aktualizacja częściowa (idempotentne, ETag) | usunięcie (kaskadowe) |
| /api/v1/books | lista książek (stronicowana) | dodaj książkę (idempotentne) | X | X | X |
| /api/v1/books/{id} | info o książce (ETag) | X | aktualizacja (ETag) | aktualizacja częściowa (idempotentne, ETag) | usunięcie |
| /api/v1/orders | lista zleceń (stronicowana) | utwórz zlecenie (idempotentne) | X | X | X |
| /api/v1/orders/bulk | X | utwórz wiele zleceń (idempotentne) | X | X | X |
| /api/v1/orders/{id} | info o zleceniu (ETag) | X | X | X | usunięcie (zwraca zasoby) |
| /api/v1/orders/{id}/return | X | X | X | zwrot zlecenia (idempotentne, ETag) | X |
| /api/v1/orders/bulk-returned | X | X | X | X | usuń zwrócone zlecenia |
| /api/v1/batch/bulk-update | X | aktualizuj wiele książek (idempotentne) | X | X | X |