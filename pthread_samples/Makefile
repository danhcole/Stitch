addToEach: addN.o
	gcc -g -Wall -pthread addN.o -o addToEach

addN.o:
	gcc -g -c -Wall -pthread addN.c

.PHONY: clean

clean:
	rm -f *.o addToEach
