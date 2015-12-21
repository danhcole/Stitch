#include "stch_headers.h"
#include <sys/time.h>	/*for getlocaltime() */
#include <time.h>


struct stch_rangeInfo_0 {
int begin;
int end;
int stepSize;
int (* a)[800];
int (* b)[800];
int (* c)[800];
int d;
int j;
int k;


};

void *_0 (void *vars) {
 int i = 0;
 for(i = ((struct stch_rangeInfo_0 *)vars)->begin; i < ((struct stch_rangeInfo_0 *)vars)->end; i++) {
{
for (((struct stch_rangeInfo_0 *)vars)->j = 0 ; ((struct stch_rangeInfo_0 *)vars)->j < 800 ; ((struct stch_rangeInfo_0 *)vars)->j = ((struct stch_rangeInfo_0 *)vars)->j + 1) {
((struct stch_rangeInfo_0 *)vars)->a[i][((struct stch_rangeInfo_0 *)vars)->j] = ((struct stch_rangeInfo_0 *)vars)->d;
((struct stch_rangeInfo_0 *)vars)->d = ((struct stch_rangeInfo_0 *)vars)->d + 1;
((struct stch_rangeInfo_0 *)vars)->b[i][((struct stch_rangeInfo_0 *)vars)->j] = ((struct stch_rangeInfo_0 *)vars)->d;
((struct stch_rangeInfo_0 *)vars)->d = ((struct stch_rangeInfo_0 *)vars)->d + 1;
}
}

}
return (void*)0;
}
struct stch_rangeInfo_1 {
int begin;
int end;
int stepSize;
int (* a)[800];
int (* b)[800];
int (* c)[800];
int d;
int j;
int k;


};

void *_1 (void *vars) {
 int i = 0;
 for(i = ((struct stch_rangeInfo_1 *)vars)->begin; i < ((struct stch_rangeInfo_1 *)vars)->end; i++) {
{
for (((struct stch_rangeInfo_1 *)vars)->j = 0 ; ((struct stch_rangeInfo_1 *)vars)->j < 800 ; ((struct stch_rangeInfo_1 *)vars)->j = ((struct stch_rangeInfo_1 *)vars)->j + 1) {
for (((struct stch_rangeInfo_1 *)vars)->k = 0 ; ((struct stch_rangeInfo_1 *)vars)->k < 800 ; ((struct stch_rangeInfo_1 *)vars)->k = ((struct stch_rangeInfo_1 *)vars)->k + 1) {
((struct stch_rangeInfo_1 *)vars)->c[i][((struct stch_rangeInfo_1 *)vars)->j] = ((struct stch_rangeInfo_1 *)vars)->c[i][((struct stch_rangeInfo_1 *)vars)->j] + ((struct stch_rangeInfo_1 *)vars)->a[i][((struct stch_rangeInfo_1 *)vars)->k] * ((struct stch_rangeInfo_1 *)vars)->b[((struct stch_rangeInfo_1 *)vars)->k][((struct stch_rangeInfo_1 *)vars)->j];
}
}
}

}
return (void*)0;
}

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

pthread_t *threadpool_0 = malloc(NUMTHREADS * sizeof(pthread_t));
struct stch_rangeInfo_0 *info_0 = malloc(sizeof(struct stch_rangeInfo_0) * NUMTHREADS);
int thread_0 = 0;
for(i = 0;i < 800;i = i+800/NUMTHREADS) {
info_0[thread_0].begin = i;
info_0[thread_0].a = a;
info_0[thread_0].b = b;
info_0[thread_0].c = c;
info_0[thread_0].d = d;
info_0[thread_0].j = j;
info_0[thread_0].k = k;

if((i + 2*(800/NUMTHREADS)) > 800) {
info_0[thread_0].end = 800;
i = 800;
}
else {
info_0[thread_0].end = i + 800/NUMTHREADS;
}
int e = pthread_create(&threadpool_0[thread_0], NULL, _0, &info_0[thread_0]);
if (e != 0) {
perror("Cannot create thread!");
free(threadpool_0); //error, free the threadpool
exit(1);
}
thread_0++;
}

//loop and wait for all the threads to finish
for(i = 0; i < NUMTHREADS; i++) {
pthread_join(threadpool_0[i], NULL);
}
//now we loop and resolve any accumulators
for(i = 0; i < NUMTHREADS; i++) {

}


pthread_t *threadpool_1 = malloc(NUMTHREADS * sizeof(pthread_t));
struct stch_rangeInfo_1 *info_1 = malloc(sizeof(struct stch_rangeInfo_1) * NUMTHREADS);
int thread_1 = 0;
for(i = 0;i < 800;i = i+800/NUMTHREADS) {
info_1[thread_1].begin = i;
info_1[thread_1].a = a;
info_1[thread_1].b = b;
info_1[thread_1].c = c;
info_1[thread_1].d = d;
info_1[thread_1].j = j;
info_1[thread_1].k = k;

if((i + 2*(800/NUMTHREADS)) > 800) {
info_1[thread_1].end = 800;
i = 800;
}
else {
info_1[thread_1].end = i + 800/NUMTHREADS;
}
int e = pthread_create(&threadpool_1[thread_1], NULL, _1, &info_1[thread_1]);
if (e != 0) {
perror("Cannot create thread!");
free(threadpool_1); //error, free the threadpool
exit(1);
}
thread_1++;
}

//loop and wait for all the threads to finish
for(i = 0; i < NUMTHREADS; i++) {
pthread_join(threadpool_1[i], NULL);
}
//now we loop and resolve any accumulators
for(i = 0; i < NUMTHREADS; i++) {

}

gettimeofday(&tval_after, NULL);
timersub(&tval_after, &tval_before, &tval_result);
printf("%s\n", "finished");
printf("Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);
return 0;
}

