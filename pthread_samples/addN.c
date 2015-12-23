// 
// addN to All Target
// December 2015
// Author: Tim Waterman

// Target C code to compile to showing adding a number to all cells
//

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

const int SIZE = 32;

struct rangeInfo {

    int begin;
    int end;
    int stepSize;
    int increment;

};

int a[SIZE];
int c[SIZE];

void printArray(int *a, int n) {

    int i;
    for(i = 0; i < n; i++) {
        printf("%d ", a[i]);
    }

    printf("\n");
}

void populateArray(int *a, int n) {

    srand(time(NULL)); //seed the RNG

    int i;
    for(i = 0; i < n; i++) {
        
        a[i] = rand() % 100;
    }
}


void *addNtoAll(void *info) { //each thread operates on a single sector of the matrices

    struct rangeInfo *myInfo = (struct rangeInfo *)info;

    printf("Hello from thread with range %d to %d\n", myInfo->begin, myInfo->end);

    int i;
    for(i = myInfo->begin; i < myInfo->end; i += myInfo->stepSize) {

        c[i] = a[i] + myInfo->increment;
    }

    return (void *)0;
}


int main(int argc, char **argv) {

    if (argc != 2) {
        fprintf(stderr ,"Usage: addToEach <number>\n");
        exit(1);
    }

    int incr;
    sscanf(argv[1], "%d", &incr);

    int numThreads = 4;

    populateArray(a, SIZE);
    printf("Initial array:\n\n");
    printArray(a, SIZE);

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
        if((i + 2*(SIZE/numThreads)) > SIZE) {
            info[thread].end = SIZE;
            i = SIZE;
        }
        else {
            info[thread].end = i + SIZE/numThreads;
        }

        info[thread].stepSize = 1;
        info[thread].increment = incr;
        
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

    printf("Final array:\n\n");
    printArray(c, SIZE);

    return 0;
}
