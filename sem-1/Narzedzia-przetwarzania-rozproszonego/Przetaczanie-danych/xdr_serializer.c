#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>
#include "student_data.h"  // Generowany z pliku .x przez rpcgen

/* 
 * Program demonstracyjny serializacji danych studentów za pomocą XDR
 * 
 * Cel: Pokazać jak używać XDR do serializacji złożonych struktur danych
 * zawierających union'y, stringi i tablice zmiennej długości
 */

int main() {
    FILE *fp;
    XDR xdrs;
    lista_studentow studenci;
    int i, j;
    
    /* Alokacja pamięci dla 3 studentów */
    studenci.lista_studentow_len = 3;
    studenci.lista_studentow_val = malloc(3 * sizeof(student));
    
    if (!studenci.lista_studentow_val) {
        fprintf(stderr, "Błąd alokacji pamięci\n");
        return 1;
    }
    
    /* Student 1: identyfikator jako nr albumu */
    studenci.lista_studentow_val[0].id.typ_id = 0;  // typ: nr albumu
    studenci.lista_studentow_val[0].id.identyfikator_u.nr_albumu = 123456;
    studenci.lista_studentow_val[0].nazwisko = strdup("Kowalski");
    studenci.lista_studentow_val[0].imie = strdup("Jan");
    
    /* Oceny dla studenta 1: DST, DB, BDB */
    studenci.lista_studentow_val[0].oceny.oceny_len = 3;
    studenci.lista_studentow_val[0].oceny.oceny_val = malloc(3 * sizeof(ocena_typ));
    studenci.lista_studentow_val[0].oceny.oceny_val[0] = DST;
    studenci.lista_studentow_val[0].oceny.oceny_val[1] = DB;
    studenci.lista_studentow_val[0].oceny.oceny_val[2] = BDB;
    
    /* Student 2: identyfikator jako email */
    studenci.lista_studentow_val[1].id.typ_id = 1;  // typ: email
    studenci.lista_studentow_val[1].id.identyfikator_u.email = strdup("anna.nowak@student.edu.pl");
    studenci.lista_studentow_val[1].nazwisko = strdup("Nowak");
    studenci.lista_studentow_val[1].imie = strdup("Anna");
    
    /* Oceny dla studenta 2: DB_P, BDB, BDB */
    studenci.lista_studentow_val[1].oceny.oceny_len = 3;
    studenci.lista_studentow_val[1].oceny.oceny_val = malloc(3 * sizeof(ocena_typ));
    studenci.lista_studentow_val[1].oceny.oceny_val[0] = DB_P;
    studenci.lista_studentow_val[1].oceny.oceny_val[1] = BDB;
    studenci.lista_studentow_val[1].oceny.oceny_val[2] = BDB;
    
    /* Student 3: identyfikator jako nr albumu */
    studenci.lista_studentow_val[2].id.typ_id = 0;  // typ: nr albumu
    studenci.lista_studentow_val[2].id.identyfikator_u.nr_albumu = 789012;
    studenci.lista_studentow_val[2].nazwisko = strdup("Wiśniewski");
    studenci.lista_studentow_val[2].imie = strdup("Piotr");
    
    /* Oceny dla studenta 3: NDST, DST, DST_P, DB */
    studenci.lista_studentow_val[2].oceny.oceny_len = 4;
    studenci.lista_studentow_val[2].oceny.oceny_val = malloc(4 * sizeof(ocena_typ));
    studenci.lista_studentow_val[2].oceny.oceny_val[0] = NDST;
    studenci.lista_studentow_val[2].oceny.oceny_val[1] = DST;
    studenci.lista_studentow_val[2].oceny.oceny_val[2] = DST_P;
    studenci.lista_studentow_val[2].oceny.oceny_val[3] = DB;
    
    /* Otwarcie pliku do zapisu w trybie binarnym */
    fp = fopen("studenci.xdr", "wb");
    if (!fp) {
        perror("Nie można otworzyć pliku do zapisu");
        return 1;
    }
    
    /* Inicjalizacja XDR stream dla operacji zapisu */
    xdrstdio_create(&xdrs, fp, XDR_ENCODE);
    
    /* Serializacja danych - funkcja xdr_lista_studentow jest generowana przez rpcgen */
    if (!xdr_lista_studentow(&xdrs, &studenci)) {
        fprintf(stderr, "Błąd serializacji XDR\n");
        return 1;
    }
    
    printf("Dane zostały zserializowane do pliku studenci.xdr\n");
    printf("Zserializowano %d studentów\n", studenci.lista_studentow_len);
    
    /* Zamknięcie stream'a i pliku */
    xdr_destroy(&xdrs);
    fclose(fp);
    
    /* Zwolnienie pamięci */
    for (i = 0; i < studenci.lista_studentow_len; i++) {
        if (studenci.lista_studentow_val[i].id.typ_id == 1) {
            free(studenci.lista_studentow_val[i].id.identyfikator_u.email);
        }
        free(studenci.lista_studentow_val[i].nazwisko);
        free(studenci.lista_studentow_val[i].imie);
        free(studenci.lista_studentow_val[i].oceny.oceny_val);
    }
    free(studenci.lista_studentow_val);
    
    return 0;
}

/*
 * Instrukcje kompilacji:
 * 1. Wygeneruj kod C z definicji XDR: rpcgen -c student_data.x > student_data_xdr.c
 * 2. Wygeneruj header: rpcgen -h student_data.x > student_data.h
 * 3. Kompiluj: gcc -o xdr_serializer xdr_serializer.c student_data_xdr.c -lnsl
 * 4. Uruchom: ./xdr_serializer
 */