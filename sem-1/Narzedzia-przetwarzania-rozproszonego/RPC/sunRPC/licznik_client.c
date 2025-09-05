#include "licznik.h"
#include <stdio.h>
#include <stdlib.h> // Potrzebne dla atoi

void
licznik_1(char *host, int wartosc)
{
	CLIENT *clnt;
	int  *result;


	clnt = clnt_create (host, LICZNIK, V1, "udp");
	if (clnt == NULL) {
		clnt_pcreateerror (host);
		exit (1);
	}


	if (wartosc >=0) {
		printf("zwieksz(%d)...\n", wartosc);
		result = zwieksz_1(&wartosc, clnt);
	} else {
		// Dla wartości ujemnych, wywołujemy 'zmniejsz' z wartością bezwzględną
        int wartosc_dodatnia = -wartosc;
        printf("zmniejsz(%d)...\n", wartosc_dodatnia);
		result = zmniejsz_1(&wartosc_dodatnia, clnt);
	}
	if (result == (int *) NULL) {
		clnt_perror (clnt, "call failed");
	} else {
		printf("Serwer odpowiedział. Nowa wartość licznika: %d\n", *result);
	}

	clnt_destroy (clnt); // zwolnienie zasobów

}


int
main (int argc, char *argv[])
{
    char *host;
    int wartosc;

    if (argc < 3) {
        printf("Sposób użycia: %s <nazwa_hosta> <wartość_do_zmiany>\n", argv[0]);
        printf("Przykład: %s localhost 10\n", argv[0]);
        printf("Przykład: %s localhost -5\n", argv[0]);
        exit(1);
    }

    host = argv[1];
    wartosc = atoi(argv[2]); // Konwertujemy drugi argument na liczbę

    licznik_1(host, wartosc);

    return 0;
exit (0);
}
