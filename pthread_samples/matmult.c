#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

const int SIZE = 3;
pthread_mutex_t lock;

struct rangeInfo {

    int begin;
    int end;
    int stepSize;
    int cols;

};

int a[SIZE][SIZE];
int b[SIZE][SIZE];
int c[SIZE][SIZE];

void printArray(int a[][SIZE], int rows, int cols) {

    int i;
    int j;
    for(i = 0; i < rows; i++) {

        for(j = 0; j < cols; j++) {

              printf("%d ", a[i][j]);
        }

        printf("\n");
    }

    printf("\n");
}

void populateArray(int a[][SIZE], int rows, int cols) {

    srand(time(NULL)); //seed the RNG

    int i;
    int j;
    for(i = 0; i < rows; i++) {

        for(j = 0; j < cols; j++) {

            printf("Assigning to [%d][%d]\n", i, j);

              a[i][j] = rand() % 10;
              b[i][j] = rand() % 10;
        }
    }
}


void *multiply(void *info) { //each thread operates on a single sector of the matrices

    struct rangeInfo *myInfo = (struct rangeInfo *)info;

    printf("Hello from thread with range %d to %d\n", myInfo->begin, myInfo->end);

    int i;
    int j, k;
    for(i = myInfo->begin; i < myInfo->end; i += myInfo->stepSize) {

        for(j = 0; j < SIZE; j++) {

            for(k = 0; k < SIZE; k++) {

                pthread_mutex_lock(&lock);
                c[i][j] += a[i][k] * b[k][j];
                pthread_mutex_unlock(&lock);
            }
        }


    }

    return (void *)0;
}


int main(int argc, char **argv) {

    int numThreads = 2;

    populateArray(a, SIZE, SIZE);
    printf("Initial array:\n\n");
    printArray(a, SIZE, SIZE);

    printf("Initial array:\n\n");
    printArray(b, SIZE, SIZE);


    //it's like deadpool, but with threads
    pthread_t *threadpool = malloc(numThreads * sizeof(pthread_t));

    if (threadpool == NULL) {
        fprintf(stderr, "Malloc failed\n");
        exit(1);
    }

    if (pthread_mutex_init(&lock, NULL) != 0) {
        fprintf(stderr, "Mutex failed\n");
        exit(1);
    }

    struct rangeInfo *info = malloc(sizeof(struct rangeInfo) * numThreads);

    int i;
    int thread = 0;

    for(i = 0; i < SIZE; i += SIZE/numThreads) {

        info[thread].begin = i;

        if((i + 2*(SIZE/numThreads)) > SIZE) {
            info[thread].end = SIZE;
        }
        else {
            info[thread].end = i + SIZE/numThreads;
        }
        info[thread].stepSize = 1;
        info[thread].cols = SIZE;
        
        int e = pthread_create(&threadpool[thread], NULL, multiply, &info[thread]);
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
    printArray(c, SIZE, SIZE);

    return 0;
}
