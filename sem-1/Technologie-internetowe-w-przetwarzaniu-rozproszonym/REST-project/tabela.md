# Operacje HTTP na Zasobach
| URI | GET | POST | PUT | PATCH | DELETE |
|-----|-----|------|-----|-------|--------|
| /authors | lista autorów | dodaj autora | X | X | X |
| /authors/{id} | info o autorze | X | aktualizacja | aktualizacja częściowa | usunięcie |
| /books | lista książek/stronicowanie | dodaj książkę | X | X | X |
| /books/{id} | info o książce | X | aktualizacja (ETag) | aktualizacja częściowa (ETag) | usunięcie |
| /orders | lista zleceń | utwórz zlecenie (idempotent) | X | X | X |
| /batch/bulk-update | X | aktualizuj wiele książek | X | X | X |
