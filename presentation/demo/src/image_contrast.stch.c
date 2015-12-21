#include "stch_headers.h"



struct stch_rangeInfo_0 {
int begin;
int end;
int stepSize;
char *buffer;
FILE * outFile;
FILE * inFile;
int *curve;


};

void *_0 (void *vars) {
 int i = 0;
 for(i = ((struct stch_rangeInfo_0 *)vars)->begin; i < ((struct stch_rangeInfo_0 *)vars)->end; i++) {
{
int temp = ((struct stch_rangeInfo_0 *)vars)->buffer[i] & 255;
((struct stch_rangeInfo_0 *)vars)->buffer[i] = ((struct stch_rangeInfo_0 *)vars)->curve[temp];
}

}
return (void*)0;
}

int main()
{
int curve[256] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 11, 11, 12, 13, 13, 14, 15, 15, 16, 17, 18, 19, 20, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 40, 41, 42, 43, 44, 45, 47, 48, 49, 50, 52, 53, 54, 55, 57, 58, 59, 61, 62, 64, 65, 66, 68, 69, 70, 72, 73, 75, 76, 78, 79, 81, 82, 83, 85, 86, 88, 89, 91, 92, 94, 96, 97, 99, 100, 102, 103, 105, 106, 108, 109, 111, 113, 114, 116, 117, 119, 120, 122, 124, 125, 127, 128, 130, 131, 133, 135, 136, 138, 139, 141, 142, 144, 146, 147, 149, 150, 152, 153, 155, 156, 158, 159, 161, 163, 164, 166, 167, 169, 170, 172, 173, 174, 176, 177, 179, 180, 182, 183, 185, 186, 187, 189, 190, 191, 193, 194, 196, 197, 198, 200, 201, 202, 203, 205, 206, 207, 208, 210, 211, 212, 213, 214, 215, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 235, 236, 237, 238, 239, 240, 240, 241, 242, 242, 243, 244, 244, 245, 246, 246, 247, 247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 252, 253, 253, 253, 254, 254, 254, 254, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255};
FILE * inFile = fopen("img.bmp", "r+");
FILE * outFile = fopen("out.bmp", "w+");
char buffer[98592];
fread(buffer, sizeof(buffer), sizeof(char), inFile);
int i = 0;

pthread_t *threadpool_0 = malloc(NUMTHREADS * sizeof(pthread_t));
struct stch_rangeInfo_0 *info_0 = malloc(sizeof(struct stch_rangeInfo_0) * NUMTHREADS);
int thread_0 = 0;
for(i = 55;i < 98592;i = i+98592/NUMTHREADS) {
info_0[thread_0].begin = i;
info_0[thread_0].buffer = buffer;
info_0[thread_0].outFile = outFile;
info_0[thread_0].inFile = inFile;
info_0[thread_0].curve = curve;

if((i + 2*(98592/NUMTHREADS)) > 98592) {
info_0[thread_0].end = 98592;
i = 98592;
}
else {
info_0[thread_0].end = i + 98592/NUMTHREADS;
}
int e = pthread_create(&threadpool_0[thread_0], NULL, _0, &info_0[thread_0]);
if (e != 0) {
perror("Cannot create thread!");
free(threadpool_0); //error, free the threadpool
exit(1);
}
thread_0++;
}

//loop and wait for all the threads to finish
for(i = 0; i < NUMTHREADS; i++) {
pthread_join(threadpool_0[i], NULL);
}
fwrite(buffer, sizeof(buffer), sizeof(char), outFile);
return 0;
}

