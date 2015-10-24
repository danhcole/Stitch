#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

const int SIZE = 10000;

struct rangeInfo {

    int begin;
    int end;
    int stepSize;

};

int a[SIZE];
int c[SIZE];

void *addNtoAll(void *info) { //each thread operates on a single sector of the matrices

    struct rangeInfo *myInfo = (struct rangeInfo *)info;

    printf("Hello from thread with range %d to %d\n", myInfo->begin, myInfo->end);

    return (void *)0;
}


int main(int argc, char **argv) {

    int numThreads = 4;

    //it's like deadpool, but with threads
    pthread_t *threadpool = malloc(numThreads * sizeof(pthread_t));

    if (threadpool == NULL) {
        perror("Malloc failed\n");
        exit(1);
    }

    struct rangeInfo *info = malloc(sizeof(struct rangeInfo) * numThreads);

    int i;
    int thread = 0;

    for(i = 0; i < SIZE; i += SIZE/numThreads) {

        info[thread].begin = i;
        info[thread].end = i + SIZE/numThreads - 1;
        info[thread].stepSize = 1;
        
        int e = pthread_create(&threadpool[thread], NULL, addNtoAll, &info[thread]);
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

    return 0;
}
