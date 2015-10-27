#include <pthread.h>

struct rangeInfo {

    int begin;
    int end;
    int stepSize;
    int increment;

};

int array a = {1, 2, 3, 4};
int array b = {10, 10, 10, 10};
int array c[4];
int acc;

void *mult(void *info) { //each thread operates on a single sector of the matrices

    struct rangeInfo *myInfo = (struct rangeInfo *)info;

    int i;
    for(i = myInfo->begin; i < myInfo->end; i += myInfo->stepSize) {

        c[i] = a[i] * b[i];
    }

    return (void *)0;
}

void *add(void *info){

    struct rangeInfo *myInfo = (struct rangeInfo *)info;

    int i;
    for(i = myInfo->begin; i < myInfo->end; i += myInfo->stepSize) {

        acc += c[i];
    }

    return (void *)0;

}

int main(){

	int numThreads = 4;

    pthread_t *threadpool = malloc(numThreads * sizeof(pthread_t));

    struct rangeInfo *info = malloc(sizeof(struct rangeInfo) * numThreads);


    int i;
    int thread = 0;

    for(i = 0; i < SIZE; i += SIZE/numThreads) {

        info[thread].begin = i;
        if((i + 2*(SIZE/numThreads)) > SIZE) {
            info[thread].end = SIZE;
            i = SIZE;
        }
        else {
            info[thread].end = i + SIZE/numThreads;
        }

        info[thread].stepSize = 1;
        info[thread].increment = incr;
        
        int e = pthread_create(&threadpool[thread], NULL, mult, &info[thread]);
        if (e != 0) {
            perror("Cannot create thread!\n");
            free(threadpool); //error, free the threadpool
            exit(1);
        }

        thread++;
    }

    //loop and wait for all the threads to finish
    for(i = 0; i < numThreads; i++) {
        pthread_join(threadpool[i], NULL);
    }

    i = 0;
    thread = 0;

    for(i = 0; i < SIZE; i += SIZE/numThreads) {

        info[thread].begin = i;
        if((i + 2*(SIZE/numThreads)) > SIZE) {
            info[thread].end = SIZE;
            i = SIZE;
        }
        else {
            info[thread].end = i + SIZE/numThreads;
        }

        info[thread].stepSize = 1;
        info[thread].increment = incr;
        
        int e = pthread_create(&threadpool[thread], NULL, add, &info[thread]);
        if (e != 0) {
            perror("Cannot create thread!\n");
            free(threadpool); //error, free the threadpool
            exit(1);
        }

        thread++;
    }

    //loop and wait for all the threads to finish
    for(i = 0; i < numThreads; i++) {
        pthread_join(threadpool[i], NULL);
    }

}