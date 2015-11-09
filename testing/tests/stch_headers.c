/*
 * stch_headers.c
 * library of standard Stitch functions
 */

 #include "stch_headers.h"

// //open()
// int  stch_open(const char* source){
// 	return fopen(source, "r+");
// }
// //write()
// int  stch_write(const int fd, stch_array* source){
// 	return fwrite(source->data, source->length, 1, fd);
// }
// //read()
// int  stch_read(const int fd, stch_array* dest){
// 	return fread(source->data, source->length, 1, fd);
// }
//print()
void stch_print(const char* s){
	printf("%s\n", s);
}
// //error()
// void stch_error(const char* e){
// 	fprintf(stderr, "%s\n", e);
// }
// //lengthof()
// int  stch_length(const stch_array* a){
// 	return a->length;
// }
// //exit()
// void stch_exit(int e){
// 	exit(e);
// }
// //cut()
// void stch_cut(int e){
// 	pthread_exit(e);
// }