/*
 * licznik_server.c - Implementacja serwera dla zdalnego licznika
 */

#include "licznik.h"
#include <stdio.h>
#include <stdlib.h>

static int licznik_wartosc = 0;

/* Implementacja funkcji zwiększającej licznik */
int * zwieksz_1_svc(int *argp, struct svc_req *rqstp) {
    static int result;
    
    printf("Zwiekszanie licznika o: %d\n", *argp);
    printf("Wartosc przed: %d\n", licznik_wartosc);
    
    licznik_wartosc += *argp;
    result = licznik_wartosc;
    
    printf("Wartosc po: %d\n", licznik_wartosc);
    
    return &result;
}

/* Implementacja funkcji zmniejszającej licznik */
int * zmniejsz_1_svc(int *argp, struct svc_req *rqstp) {
    static int result;
    
    printf("Zmniejszanie licznika o: %d\n", *argp);
    printf("Wartosc przed: %d\n", licznik_wartosc);
    
    licznik_wartosc -= *argp;
    result = licznik_wartosc;
    
    printf("Wartosc po: %d\n", licznik_wartosc);
    
    return &result;
}