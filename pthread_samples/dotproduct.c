// 
// Dot Product Target
// December 2015
// Author: Tim Waterman

// Target C code to compile to showing the dot product
//

#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>

struct rangeInfo {

    int begin;
    int end;
    int stepSize;
    int increment;

};

int a[] = {1, 2, 3, 4};
int b[] = {10, 10, 10, 10};
int c[] = {0, 0, 0, 0};
int acc;
int SIZE = 4;

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

    int j;
    for (j = 0; j < 4; j++){
    	fprintf(stderr, "%d ", c[j]);
    }
    fprintf(stderr, "\n");

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
        info[thread].increment = 1;
        
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
        info[thread].increment = 1;
        
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

    for (j = 0; j < 4; j++){
    	fprintf(stderr, "%d ", c[j]);
    }
    fprintf(stderr, "\n");

    return 0;
}