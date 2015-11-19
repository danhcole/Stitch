CC = gcc
CXX = g++

INCLUDES = -g -Wall #-I

CFLAGS = $(INCLUDES)
CXXFLAGS = $(INCLUDES)

LDFLAGS = -g #-L
LDLIBS = 

stch_headers_LIB: stch_headers.o
	ar rc libstch_headers.a stch_headers.o
	ranlib libstch_headers.a

stch_headers.o:

.PHONY: clean
clean:
	rm -f *.o a.out core libstch_headers.a stch_headers.o

.PHONY: all
all: clean stch_headers_LIB
