#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv)
{
    volatile int* ptr = malloc(sizeof(int));
    volatile int x = 0;
    int i = 0;
    int d   = atoi(argv[1]);
    int max = atoi(argv[2]);
    *ptr = 0;
    for(int i=0; i<max; i++) {
         *ptr += d;
    }
    printf("%d\n", *ptr);
    printf("%d\n", max);
    return 0;
}
