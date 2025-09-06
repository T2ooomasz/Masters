#include "licznik.h"

static int licznik_val = 0;

int *
zwieksz_1_svc(int *argp, struct svc_req *rqstp)
{
	static int  result;

	printf("zwieksz o %d\n", *argp);

	licznik_val = licznik_val + *argp;
	result = licznik_val;

	printf("licznik: %d\n", result);


	return &result;
}

int *
zmniejsz_1_svc(int *argp, struct svc_req *rqstp)
{
	static int  result;

	printf("zmniejsz o %d\n", *argp);

	licznik_val = licznik_val - *argp;
	result = licznik_val;

	printf("licznik: %d\n", result);

	return &result;
}
