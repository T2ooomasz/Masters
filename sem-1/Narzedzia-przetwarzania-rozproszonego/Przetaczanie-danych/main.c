#include "studenci.h"
#include <rpc/xdr.h>
#include <stdio.h>

int main() {
    Student student;
    
    /* Ustawienie identyfikatora - numer albumu */
    student.identyfikator.typ = NUMER_ALBUMU;
    student.identyfikator.IdentyfikatorStudenta_u.numer_albumu = 12345;
    
    /* Ustawienie danych osobowych */
    student.nazwisko = "Kowalski";
    student.imie = "Jan";
    
    /* Ustawienie ocen */
    Ocena oceny_tab[] = {DB, BDB, DST_P};
    student.oceny.oceny_len = 3;
    student.oceny.oceny_val = oceny_tab;
    
    /* Serializacja do pliku */
    FILE *fp = fopen("student.dat", "wb");
    XDR xdrs;
    xdrstdio_create(&xdrs, fp, XDR_ENCODE);
    
    if (!xdr_Student(&xdrs, &student)) {
        printf("Błąd serializacji\n");
    }
    
    xdr_destroy(&xdrs);
    fclose(fp);
    
    return 0;
}

/* Deserializacja z pliku */
/*
FILE *fp = fopen("student.dat", "rb");
XDR xdrs;
Student student;

xdrstdio_create(&xdrs, fp, XDR_DECODE);

if (xdr_Student(&xdrs, &student)) {
    printf("Student: %s %s\n", student.imie, student.nazwisko);
    
    if (student.identyfikator.typ == NUMER_ALBUMU) {
        printf("Numer albumu: %u\n", 
               student.identyfikator.IdentyfikatorStudenta_u.numer_albumu);
    }
    
    printf("Liczba ocen: %u\n", student.oceny.oceny_len);
}

xdr_destroy(&xdrs);
fclose(fp);
*/
