/*
 * Definicje XDR dla systemu zarządzania danymi studentów
 * Plik: studenci.x
 */

/* Definicja możliwych ocen */
enum Ocena {
    NDST = 0,    /* niedostateczny */
    DST = 1,     /* dostateczny */
    DST_P = 2,   /* dostateczny plus */
    DB = 3,      /* dobry */
    DB_P = 4,    /* dobry plus */
    BDB = 5      /* bardzo dobry */
};

/* Typ określający rodzaj identyfikatora */
enum TypIdentyfikatora {
    NUMER_ALBUMU = 1,
    ADRES_EMAIL = 2
};

/* Union dla identyfikatora studenta */
union IdentyfikatorStudenta switch (TypIdentyfikatora typ) {
    case NUMER_ALBUMU:
        unsigned int numer_albumu;
    case ADRES_EMAIL:
        string email<>;
    default:
        void;
};

/* Struktura reprezentująca jednego studenta */
struct Student {
    IdentyfikatorStudenta identyfikator;
    string nazwisko<20>;        /* max 20 znaków */
    string imie<20>;           /* max 20 znaków */
    Ocena oceny<>;             /* tablica ocen bez ograniczenia */
};

/* Tablica studentów - główny typ dla serializacji */
typedef Student ListaStudentow<>;