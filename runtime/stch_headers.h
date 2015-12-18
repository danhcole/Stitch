/*
 * stch_headers.h
 * auto-included in ever c file written by the Stitch compiler
 */

#ifndef __STCH_HEADERS_H__
#define __STCH_HEADERS_H__

/*
************
* Includes *
************
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>

/*
***********
* Defines *
***********
*/
#define NUMTHREADS 4

/*
***********
* Structs *
***********
*/

//hold local variables to pass from the stitch loop into a thread
//need to figure this out...
struct stch_LocalVars{
    
    void 		  *vars;
    unsigned int  n;

};

// //range info passed into the thread
// struct stch_rangeInfo{

//     int 	begin;
//     int 	end;
//     int 	stepSize;
//     int 	cols;
//     struct 	stchLocalVars *locals;
//     void *myvars;

// };

//array wrapper
typedef struct stch_array{

	void 			*data;
	unsigned int 	length;

} stch_array;

/*
************************
* Function definitions *
************************
*/

//open()
int  stch_open(const char* source);
// //write()
int  stch_write(const int fd, stch_array* source);
// //read()
int  stch_read(const int fd, stch_array* dest);
// //lengthof()
int  stch_length(const stch_array* a);
// //cut()
void stch_cut(void* e);


#endif
