#include "licznik.h"

static int licznik_val = 0;

int *
zwieksz_1_svc(int *argp, struct svc_req *rqstp)
{
	static int  result;

	printf("zwieksz(%d)\n", *argp);
	licznik_val += *argp;
	result = licznik_val;
	return &result;
}

int *
zmniejsz_1_svc(int *argp, struct svc_req *rqstp)
{
	static int  result;

	printf("zmniejsz(%d)\n", *argp);
	licznik_val -= *argp;
	result = licznik_val;
	return &result;
}

