/*
 * stch_headers.c
 * library of standard Stitch functions
 */

 #include "stch_headers.h"

//open()
// int  stch_open(const char* source){
// 	return fopen(source, "r+");
// }
//write()
// int  stch_write(const int fd, stch_array* source){
// 	return write(source->data, source->length, 1, fd);
// }
//read()
// int  stch_read(const int fd, stch_array* dest){
// 	return read(source->data, source->length, 1, fd);
// }

//lengthof()
int  stch_length(const stch_array* a){
	return a->length;
}
//exit()
void stch_exit(int e){
	exit(e);
}
//cut()
void stch_cut(void* e){
	pthread_exit(e);
}