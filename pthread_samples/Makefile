all: addToEach matmult

matmult: matmult.o
	gcc -g -Wall -pthread matmult.o -o matmult

matmult.o: matmult.c
	gcc -g -c -Wall -pthread matmult.c


addToEach: addN.o
	gcc -g -Wall -pthread addN.o -o addToEach

addN.o: addN.c
	gcc -g -c -Wall -pthread addN.c

.PHONY: clean

clean:
	rm -f *.o addToEach matmult
