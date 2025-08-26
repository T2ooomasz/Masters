#include "licznik.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void licznik_prog_1(char *host, char *op, int val)
{
	CLIENT *clnt;
	int  *result;

#ifndef	DEBUG
	clnt = clnt_create (host, LICZNIK_PROG, V1, "udp");
	if (clnt == NULL) {
		clnt_pcreateerror (host);
		exit (1);
	}
#endif	/* DEBUG */

    if (strcmp(op, "zwieksz") == 0) {
	    result = zwieksz_1(&val, clnt);
    } else if (strcmp(op, "zmniejsz") == 0) {
        result = zmniejsz_1(&val, clnt);
    } else {
        fprintf(stderr, "Unknown operation: %s\n", op);
        exit(1);
    }

	if (result == (int *) NULL) {
		clnt_perror (clnt, "call failed");
	} else {
		printf("wynik = %d\n", *result);
	}
#ifndef	DEBUG
	clnt_destroy (clnt);
#endif	 /* DEBUG */
}


int main (int argc, char *argv[])
{
	char *host;
    char *op;
    int val;

	if (argc < 4) {
		printf ("usage: %s server_host operation value\n", argv[0]);
		exit (1);
	}
	host = argv[1];
    op = argv[2];
    val = atoi(argv[3]);
	licznik_prog_1 (host, op, val);
exit (0);
}
