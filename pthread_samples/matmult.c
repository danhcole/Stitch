#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

const int SIZE = 10000;

int a[SIZE];
int c[SIZE];

void *addNtoAll(void *index) { //each thread operates on a single sector of the matrices

    int count = (int *)index;

    int i;
    c[count] = a[count] + 1;

    }
}


int main(int argc, char **argv) {


    //it's like deadpool, but with threads
    pthread_t *threadpool = malloc(SIZE * sizeof(pthread_t));

    if (threadpool == NULL) {
        perror("Malloc failed\n");
        exit(1);
    }

    for(i = 0; i < SIZE; i++) {
        
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
