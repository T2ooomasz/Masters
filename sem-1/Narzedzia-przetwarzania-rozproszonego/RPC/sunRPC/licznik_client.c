/*
 * licznik_client.c - Klient dla zdalnego licznika
 */

#include "licznik.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void test_licznika(char *host)
{
    CLIENT *clnt;  // Wskaźnik na strukturę klienta RPC
    int *wynik;
    int wartosc;

    printf("=== Klient RPC - Test zdalnego licznika ===\n");
    printf("Łączenie z serwerem: %s\n\n", host);
    
    /* Utworzenie klienta RPC 
     * - host: adres serwera
     * - LICZNIK: numer programu (z licznik.x)
     * - V1: wersja programu (z licznik.x) 
     * - "udp": protokół transportowy
     */
    clnt = clnt_create(host, LICZNIK, V1, "udp");
    if (clnt == NULL) {
        clnt_pcreateerror(host);
        printf("Błąd: Nie można połączyć się z serwerem RPC\n");
        exit(1);
    }

    /* Test 1: Zwiększenie o 5 */
    wartosc = 5;
    printf("Klient: Wywołuję zwieksz(%d)\n", wartosc);
    wynik = zwieksz_1(&wartosc, clnt);
    if (wynik == NULL) {
        clnt_perror(clnt, "Błąd wywołania zwieksz");
    } else {
        printf("Klient: Otrzymałem wynik: %d\n\n", *wynik);
    }

    /* Test 2: Zwiększenie o 10 */
    wartosc = 10;
    printf("Klient: Wywołuję zwieksz(%d)\n", wartosc);
    wynik = zwieksz_1(&wartosc, clnt);
    if (wynik == NULL) {
        clnt_perror(clnt, "Błąd wywołania zwieksz");
    } else {
        printf("Klient: Otrzymałem wynik: %d\n\n", *wynik);
    }

    /* Test 3: Zmniejszenie o 3 */
    wartosc = 3;
    printf("Klient: Wywołuję zmniejsz(%d)\n", wartosc);
    wynik = zmniejsz_1(&wartosc, clnt);
    if (wynik == NULL) {
        clnt_perror(clnt, "Błąd wywołania zmniejsz");
    } else {
        printf("Klient: Otrzymałem wynik: %d\n\n", *wynik);
    }

    /* Test 4: Zmniejszenie o 7 */
    wartosc = 7;
    printf("Klient: Wywołuję zmniejsz(%d)\n", wartosc);
    wynik = zmniejsz_1(&wartosc, clnt);
    if (wynik == NULL) {
        clnt_perror(clnt, "Błąd wywołania zmniejsz");
    } else {
        printf("Klient: Otrzymałem wynik: %d\n\n", *wynik);
    }

    /* Test 5: Zwiększenie o 20 */
    wartosc = 20;
    printf("Klient: Wywołuję zwieksz(%d)\n", wartosc);
    wynik = zwieksz_1(&wartosc, clnt);
    if (wynik == NULL) {
        clnt_perror(clnt, "Błąd wywołania zwieksz");
    } else {
        printf("Klient: Otrzymałem wynik: %d\n\n", *wynik);
    }

    /* Zwolnienie zasobów klienta */
    clnt_destroy(clnt);
    printf("Klient: Połączenie z serwerem zakończone\n");
}

int main(int argc, char *argv[])
{
    if (argc != 2) {
        printf("Użycie: %s <adres_serwera>\n", argv[0]);
        printf("Przykład: %s localhost\n", argv[0]);
        exit(1);
    }
    
    uruchom_klienta(argv[1]);
    return 0;
}