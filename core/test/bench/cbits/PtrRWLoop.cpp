#include <stdio.h>
#include <stdlib.h>

extern "C" {

int c_ptr_rwloop (int d, int iterations)
{
    volatile int* ptr = (int*)malloc(sizeof(*ptr));
    *ptr = 0;
    for(int i=0; i<iterations; i++) {
         *ptr += d;
    }
    return *ptr;
}

}