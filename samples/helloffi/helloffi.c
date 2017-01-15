#include <stdio.h>
#include "helloffi.h"

int
testfunc(int arg1, int arg2){
    printf("Hello. arg1 = %d, arg2 = %d\n",
           arg1, arg2);
}
