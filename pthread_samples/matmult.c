#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

const int SIZE = 3;
int A[SIZE][SIZE] = {1,2,3,4,5,6,7,8,9} //first argument to multiply
int B[SIZE][SIZE] = {9,8,7,6,5,4,3,2,1} //second argument to multiply
int R[SIZE][SIZE]; //result matrix

void *multiply(void *sector) { //each thread operates on a single sector of the matrices

    int sec = (int *)sector;

//do multiplication here, using sector the determine the row & column

    int i = sec;

    for(i = 0; i < SIZE; i++) {

        C[sec][sec] = A[sec][i] * B[i][sec];
    }

    return 0;
}


int main(int argc, char **argv) {


    //it's like deadpool, but with threads
    pthread_t *threadpool = malloc(SIZE *SIZE * sizeof(pthread_t));

    if (threadpool == NULL) {
        perror("Malloc failed\n");
        exit(1);
    }

    for(i = 0; i < SIZE * SIZE; i++) {
        
        int e = pthread_create(&threadpool[i], NULL, multiply, (void *)i);
        if (e != 0) {
            perror("Cannot create thread!\n");
            free(threadpool);
            exit(1);
        }
    }

    //loop and wait for all the threads to finish
    for(i = 0; i < SIZE * SIZE; i++) {
        pthread_join(&threadpool[i], NULL);
    }


    return 0;
}
