#include "stch_headers.h"
#include <sys/time.h>	/*for getlocaltime() */
#include <time.h>


int main()
{
int a[800][800];
int b[800][800];
int c[800][800];
int d = 1;
int i = 0;
int j = 0;
int k = 0;
struct timeval tval_before, tval_after, tval_result;
gettimeofday(&tval_before, NULL);

for (i = 0 ; (i < 800) ; i = (i + 1)) {
for (j = 0 ; (j < 800) ; j = (j + 1)) {
a[i][j] = d;
d = (d + 1);
b[i][j] = d;
d = (d + 1);
}
}
for (i = 0 ; (i < 800) ; i = (i + 1)) {
for (j = 0 ; (j < 800) ; j = (j + 1)) {
for (k = 0 ; (k < 800) ; k = (k + 1)) {
c[i][j] = (c[i][j] + (a[i][k] * b[k][j]));
}
}
}

gettimeofday(&tval_after, NULL);
timersub(&tval_after, &tval_before, &tval_result);
printf("%s\n", "finished");
printf("Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);
return 0;
}

