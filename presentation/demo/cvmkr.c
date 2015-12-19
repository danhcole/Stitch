#include <stdio.h>

int main(){

    printf("{ ");
    int i;
    for (i = 255; i > 1; i--){
        printf("%d, ", i);
    }
    printf("0 }");
    return 0;
}
