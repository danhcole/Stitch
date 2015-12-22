/* Image Inverter */

#include <stdio.h>

int main(){

	int curve[256] = { 0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,7,7,8,8,9,9,10,11,11,12,13,13,14,15,15,16,17,18,19,20,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,40,41,42,43,44,45,47,48,49,50,52,53,54,55,57,58,59,61,62,64,65,66,68,69,70,72,73,75,76,78,79,81,82,83,85,86,88,89,91,92,94,96,97,99,100,102,103,105,106,108,109,111,113,114,116,117,119,120,122,124,125,127,128,130,131,133,135,136,138,139,141,142,144,146,147,149,150,152,153,155,156,158,159,161,163,164,166,167,169,170,172,173,174,176,177,179,180,182,183,185,186,187,189,190,191,193,194,196,197,198,200,201,202,203,205,206,207,208,210,211,212,213,214,215,217,218,219,220,221,222,223,224,225,226,227,228,229,230,231,232,233,234,235,235,236,237,238,239,240,240,241,242,242,243,244,244,245,246,246,247,247,248,248,249,249,250,250,251,251,252,252,252,253,253,253,254,254,254,254,255,255,255,255,255,255,255,255,255,255,255 };
	
	FILE *input = fopen("img.bmp", "r");
	FILE *output = fopen("out.bmp", "w");

	fseek(input, 0, SEEK_END);
	int sz = ftell(input);
	rewind(input);

	char buffer[sz];

	fread(buffer, 1, sz, input);

	int i;
	for (i = 55; i < sz; i++){
		int temp = buffer[i] & 255;
		fprintf(stderr, "Buffer[i]: %d\tInput: %x\tOutput: %x\n",buffer[i], temp, curve[temp]);
		buffer[i] = curve[temp];
	}

	fwrite(buffer, 1, sz, output);

	fclose(input);
	fclose(output);
}