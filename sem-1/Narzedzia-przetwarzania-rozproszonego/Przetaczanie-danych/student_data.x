/* student_data.x - Definicja struktur danych dla serializacji XDR */

/* Definicja typów ocen jako enum dla bezpieczeństwa typów */
enum ocena_typ {
    NDST = 0,
    DST = 1,
    DST_P = 2,
    DB = 3,
    DB_P = 4,
    BDB = 5
};

/* Union dla identyfikatora - może być nr albumu lub emailem */
union identyfikator switch (int typ_id) {
    case 0:
        unsigned int nr_albumu;    /* nr albumu jako liczba bez znaku */
    case 1:
        string email<>;            /* email bez ograniczenia długości */
};

/* Struktura główna studenta */
struct student {
    identyfikator id;              /* identyfikator (union) */
    string nazwisko<20>;           /* nazwisko max 20 znaków */
    string imie<20>;               /* imię max 20 znaków */
    ocena_typ oceny<>;             /* tablica ocen zmiennej długości */
};

/* Tablica studentów zmiennej długości */
typedef student lista_studentow<>;

/* Program XDR dla generowania funkcji serializacji/deserializacji */
program STUDENT_PROG {
    version STUDENT_VERS {
        void SERIALIZE_STUDENTS(lista_studentow) = 1;
    } = 1;
} = 0x20000001;