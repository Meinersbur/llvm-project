// RUN:
// REQUIRES: fixing

#include <omp.h>
#include <stdio.h>

void test(long long i) {
	printf("Hello world from iteration %lld in thread %d\n", i, omp_get_thread_num() );
}
