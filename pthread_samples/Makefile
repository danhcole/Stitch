matmult: matmult.o
	gcc -g -Wall -pthread matmult.o -o matmult

matmult.o:
	gcc -g -c -Wall -pthread matmult.c

.PHONY: clean

clean:
	rm -f *.o matmult
