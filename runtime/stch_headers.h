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
#include <stdout.h>
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
struct stchLocalVars{

};

//range info passed into the thread
struct rangeInfo {

    int begin;
    int end;
    int stepSize;
    int cols;

};

#endif
