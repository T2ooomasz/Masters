# Operacje HTTP na Zasobach

| URI | GET | POST | PUT | PATCH | DELETE |
|-----|-----|------|-----|-------|--------|
| `/authors` | lista autorów/stronicowanie | dodaj autora | X | X | X |
| `/authors/{id}` | info o autorze | X | aktualizacja | aktualizacja częściowa | usuń autora |
| `/books` | lista książek/stronicowanie | dodaj książkę | X | X | X |
| `/books/{id}` | info o książce | X | aktualizacja (ETag) | aktualizacja częściowa (ETag) | usuń książkę |
| `/books/{id}/reviews` | lista recenzji | dodaj recenzję | X | X | X |
| `/books/{id}/reviews/{rid}` | info o recenzji | X | aktualizacja recenzji | aktualizacja częściowa | usuń recenzję |
| `/users` | lista użytkowników | rejestracja użytkownika | X | X | X |
| `/users/{id}` | info o użytkowniku | X | aktualizacja | aktualizacja częściowa | zamknij konto |
| `/users/{id}/loans` | lista wypożyczeń | X | X | X | X |
| `/loans` | lista wypożyczeń | wypożycz książkę | X | X | X |
| `/loans/{id}` | info o wypożyczeniu | X | przedłużenie/zwrot (ETag) | przedłużenie/zwrot częściowe | anuluj wypożyczenie |
| `/orders` | lista zleceń | złóż zlecenie (Idempotency-Key) | X | X | X |
| `/orders/{id}` | info o zleceniu | X | X | X | anuluj zlecenie |
| `/batch-operations/bulk-loan` | X | masowe wypożyczenie | X | X | X |
| `/batch-operations/bulk-return` | X | masowy zwrot | X | X | X |
| `/batch-operations/inventory-update` | X | aktualizacja inwentarza | X | X | X |
