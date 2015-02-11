/* Primitives test */

#include <stdio.h>
#include <stdint.h>

#if defined(_WIN32)||defined(__CYGWIN__)
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#endif

EXPORT
void
test0_print_and_fill(uint64_t* in, int in_len, uint64_t* out, int out_len){
    int i;
    printf("test0_print_and_fill: in:%p in_len:%d out:%p out_len: %d\n",
           in,in_len,out,out_len);
    for(i = 0;i != in_len;i++){
        printf("in[%d] = %ld\n",i,in[i]);
    }
    for(i = 0; i != out_len; i++){
        out[i] = i+1;
    }
}
