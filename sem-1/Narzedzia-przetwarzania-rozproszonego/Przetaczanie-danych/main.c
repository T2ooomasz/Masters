/* main.c - Poprawny przykład użycia z rpcgen */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "studenci.h"

void print_student(const Student *student) {
    printf("=== STUDENT ===\n");
    printf("Imię: %s\n", student->imie);
    printf("Nazwisko: %s\n", student->nazwisko);
    
    if (student->identyfikator.typ == NUMER_ALBUMU) {
        printf("Numer albumu: %u\n", 
               student->identyfikator.IdentyfikatorStudenta_u.numer_albumu);
    } else if (student->identyfikator.typ == ADRES_EMAIL) {
        printf("Email: %s\n", 
               student->identyfikator.IdentyfikatorStudenta_u.email);
    }
    
    printf("Liczba ocen: %u\n", student->oceny.oceny_len);
    printf("Oceny: ");
    for (u_int i = 0; i < student->oceny.oceny_len; i++) {
        switch(student->oceny.oceny_val[i]) {
            case NDST: printf("ndst "); break;
            case DST: printf("dst "); break;
            case DST_P: printf("dst+ "); break;
            case DB: printf("db "); break;
            case DB_P: printf("db+ "); break;
            case BDB: printf("bdb "); break;
        }
    }
    printf("\n\n");
}

int main() {
    printf("=== DEMO: System XDR dla studentów ===\n\n");
    
    /* === TWORZENIE STUDENTA 1 (numer albumu) === */
    Student student1;
    memset(&student1, 0, sizeof(Student));
    
    // Identyfikator - numer albumu
    student1.identyfikator.typ = NUMER_ALBUMU;
    student1.identyfikator.IdentyfikatorStudenta_u.numer_albumu = 12345;
    
    // Dane osobowe
    student1.nazwisko = strdup("Kowalski");
    student1.imie = strdup("Jan");
    
    // Oceny
    Ocena oceny1[] = {DB, BDB, DST_P};
    student1.oceny.oceny_len = 3;
    student1.oceny.oceny_val = malloc(3 * sizeof(Ocena));
    memcpy(student1.oceny.oceny_val, oceny1, 3 * sizeof(Ocena));
    
    /* === TWORZENIE STUDENTA 2 (email) === */
    Student student2;
    memset(&student2, 0, sizeof(Student));
    
    // Identyfikator - email
    student2.identyfikator.typ = ADRES_EMAIL;
    student2.identyfikator.IdentyfikatorStudenta_u.email = 
        strdup("anna.nowak@student.pw.edu.pl");
    
    // Dane osobowe
    student2.nazwisko = strdup("Nowak");
    student2.imie = strdup("Anna");
    
    // Oceny
    Ocena oceny2[] = {BDB, BDB, DB_P, BDB};
    student2.oceny.oceny_len = 4;
    student2.oceny.oceny_val = malloc(4 * sizeof(Ocena));
    memcpy(student2.oceny.oceny_val, oceny2, 4 * sizeof(Ocena));
    
    /* === TWORZENIE LISTY STUDENTÓW === */
    ListaStudentow lista;
    lista.ListaStudentow_len = 2;
    lista.ListaStudentow_val = malloc(2 * sizeof(Student));
    lista.ListaStudentow_val[0] = student1;
    lista.ListaStudentow_val[1] = student2;
    
    printf("Utworzono listę %u studentów:\n", lista.ListaStudentow_len);
    for (u_int i = 0; i < lista.ListaStudentow_len; i++) {
        print_student(&lista.ListaStudentow_val[i]);
    }
    
    /* === SERIALIZACJA DO PLIKU === */
    printf("=== SERIALIZACJA ===\n");
    FILE *fp = fopen("studenci.dat", "wb");
    if (!fp) {
        printf("Błąd: nie można otworzyć pliku do zapisu!\n");
        return 1;
    }
    
    XDR xdrs;
    xdrstdio_create(&xdrs, fp, XDR_ENCODE);
    
    if (!xdr_ListaStudentow(&xdrs, &lista)) {
        printf("Błąd serializacji!\n");
        xdr_destroy(&xdrs);
        fclose(fp);
        return 1;
    }
    
    xdr_destroy(&xdrs);
    fclose(fp);
    printf("Dane zapisane do pliku 'studenci.dat'\n\n");
    
    /* === DESERIALIZACJA Z PLIKU === */
    printf("=== DESERIALIZACJA ===\n");
    fp = fopen("studenci.dat", "rb");
    if (!fp) {
        printf("Błąd: nie można otworzyć pliku do odczytu!\n");
        return 1;
    }
    
    ListaStudentow loaded_lista;
    memset(&loaded_lista, 0, sizeof(ListaStudentow));
    
    xdrstdio_create(&xdrs, fp, XDR_DECODE);
    
    if (!xdr_ListaStudentow(&xdrs, &loaded_lista)) {
        printf("Błąd deserializacji!\n");
        xdr_destroy(&xdrs);
        fclose(fp);
        return 1;
    }
    
    xdr_destroy(&xdrs);
    fclose(fp);
    
    printf("Wczytano listę %u studentów z pliku:\n", loaded_lista.ListaStudentow_len);
    for (u_int i = 0; i < loaded_lista.ListaStudentow_len; i++) {
        print_student(&loaded_lista.ListaStudentow_val[i]);
    }
    
    /* === WERYFIKACJA === */
    printf("=== WERYFIKACJA ===\n");
    printf("Oryginalna lista: %u studentów\n", lista.ListaStudentow_len);
    printf("Wczytana lista: %u studentów\n", loaded_lista.ListaStudentow_len);
    
    if (lista.ListaStudentow_len == loaded_lista.ListaStudentow_len) {
        printf("✓ Liczba studentów się zgadza!\n");
        
        for (u_int i = 0; i < lista.ListaStudentow_len; i++) {
            Student *orig = &lista.ListaStudentow_val[i];
            Student *load = &loaded_lista.ListaStudentow_val[i];
            
            if (strcmp(orig->imie, load->imie) == 0 && 
                strcmp(orig->nazwisko, load->nazwisko) == 0 &&
                orig->identyfikator.typ == load->identyfikator.typ &&
                orig->oceny.oceny_len == load->oceny.oceny_len) {
                printf("✓ Student %u: dane podstawowe OK\n", i+1);
            } else {
                printf("✗ Student %u: różnica w danych!\n", i+1);
            }
        }
    } else {
        printf("✗ Różna liczba studentów!\n");
    }
    
    printf("\n=== SUKCES! System XDR działa poprawnie ===\n");
    
    /* === ZWOLNIENIE PAMIĘCI === */
    // Zwolnienie oryginalnej listy
    free(student1.nazwisko);
    free(student1.imie);
    free(student1.oceny.oceny_val);
    free(student2.nazwisko);
    free(student2.imie);
    free(student2.identyfikator.IdentyfikatorStudenta_u.email);
    free(student2.oceny.oceny_val);
    free(lista.ListaStudentow_val);
    
    // Zwolnienie wczytanej listy (rpcgen używa xdr_free)
    xdr_free((xdrproc_t)xdr_ListaStudentow, (char*)&loaded_lista);
    
    return 0;
}