#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>
#include <yuniffi/abiv0/bootstrap.h>

typedef void (*yuniffi_nccc_func_t)(uint64_t* in, int in_count,
                                    uint64_t* out, int out_count);

void
do_malloc(uint64_t size, uint64_t* out_ptr){
    void* p;
    p = malloc(size);
    //printf("malloc: %lx => %p\n",size,p);
    *out_ptr = (uintptr_t)p;
}

void
do_free(uint64_t ptr){
    //printf("free: %lx\n",ptr);
    free((void*)(uintptr_t)ptr);
}

void
do_dlopen(uint64_t ptr_name, uint64_t* out_err, uint64_t* out_ptr){
    const char* nam = (const char*)(uintptr_t)ptr_name;
    void* p;
    p = dlopen(nam, RTLD_NOW);
    if(p){
        *out_err = 0;
        *out_ptr = (uintptr_t)p;
    }else{
        *out_err = (uintptr_t)dlerror();
        *out_ptr = 0;
    }
}

void
do_dlsym(uint64_t handle, uint64_t ptr_name, uint64_t* out_err, 
         uint64_t* out_ptr){
    const char* nam = (const char*)(uintptr_t)ptr_name;
    void* p;
    p = dlsym((void*)(uintptr_t)handle, nam);
    if(p){
        *out_err = 0;
        *out_ptr = (uintptr_t)p;
    }else{
        *out_err = (uintptr_t)dlerror();
        *out_ptr = 0;
    }
}

static void
yuniffi_bootstrap0(uint64_t* in, int in_len, uint64_t* out, int out_len){
    int func = in[0];
    switch(func){
        case YUNIBOOTSTRAP0_MALLOC:
            do_malloc(in[1],&out[0]);
            break;
        case YUNIBOOTSTRAP0_FREE:
            do_free(in[1]);
            break;
        case YUNIBOOTSTRAP0_DLOPEN:
            do_dlopen(in[1],&out[0],&out[1]);
            break;
        case YUNIBOOTSTRAP0_DLSYM:
            do_dlsym(in[1],in[2],&out[0],&out[1]);
            break;
        default:
            /* Do nothing */
            break;
    }
}
