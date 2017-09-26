#include <stdint.h>
#include <stdio.h> /* For NULL */
#define TESTING_H_INCLUDED /* Fake */
#include "testinterface.h"

const char*
test_constcharptr(void){
    return NULL;
}

int 
test_constcharptr_check(int in){
    return 1;
}

const void*
test_constvoidptr(void){
    return NULL;
}

int
test_constvoidptr_check(void* in){
    return 1;
}

int
test_intvec(int* in, int count, int size){
    return 1;
}

int
test_someblob_t_1(someblob_t* b){
    return 1;
}

int
test_someblob_t_2(someblob_t* b){
    return 1;
}

int
test_someunion_u_1(union someunion_u* in){
    return 1;
}

int
test_someunion_u_1a(union someunion_u* in){
    return 1;
}

int
test_someunion_u_2(union someunion_u* out){
    return 1;
}

int
test_somestruct_1(struct somestruct_s* in){
    return 1;
}

int
test_somestruct_2(struct somestruct_s* out){
    return 1;
}

static int
test_somestruct_1f(struct somestruct_s* in){
}

void*
test_get_somestruct_1f_test(void){
    return NULL;
}

int
testecho_intecho(int a){
    return a;
}

int
test_outint(int* thevalue){
    return 1;
}

typedef void (*the_trampoline_t)(uintptr_t proc,
                                 uint64_t* in, int in_len,
                                 uint64_t* out, int out_len);

extern the_trampoline_t testing_trivial_export_constants_callback_ptr;

int
test_callcallback(void* cb, int a, int b){
    uint64_t in[2];
    uint64_t out[1];
    in[0] = a;
    in[1] = b;
    out[0] = 0;

    printf("Calling %p\n", testing_trivial_export_constants_callback_ptr);

    testing_trivial_export_constants_callback_ptr((uintptr_t)cb, 
                                                  in, 2, out, 1);

    return out[0];
}

int CONST_1 = 1234;
int MINUS_CONST_1 = -1234;
float REAL_1 = 0.5;
int CONST_2 = 1234;
float REAL_2 = 0.5;

int size_of_someblob_t = sizeof(someblob_t);
int size_of_someunion_u = sizeof(union someunion_u);
int size_of_somestruct_s = sizeof(struct somestruct_s);
int size_of_someopaque_t = sizeof(someopaque_t);

