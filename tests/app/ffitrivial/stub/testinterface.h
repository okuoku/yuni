#ifndef TESTING_H_INCLUDED
#error huh?
#endif

#ifndef NULL
#error Where is my stdio.h ??
#endif

/* Actual testinterface */
#ifndef __TESTINTERFACE_H
#define __TESTINTERFACE_H

#ifdef __cplusplus
extern "C" {
/* } */
#endif

/* Types: struct / union */

enum someenum_e {
    ENUM_VAL_1,
    ENUM_VAL_2
};

typedef enum someenum_e someenum_t;

enum someenum2_e {
    ENUM2_VAL_1 = 9999,
    ENUM2_VAL_2
};

struct someopaque_s {
    int a;
};

typedef struct someopaque_s someopaque_t;

struct someblob_s {
    int a;
    int b;
};

typedef struct someblob_s someblob_t;

union someunion_u {
    int a;
    int b;
};

struct somestruct_s {
    float fnum;
    int someinteger;
    int someintegers[4];
    int bufferlength;
    void* buffer;
    int someconst;
    enum someenum2_e someenum2;
    int someintlength;
    int someintbytelength;
    int* someints;
    int* intz;
};

/* Functions */

const char* test_constcharptr(void);
int test_constcharptr_check(int in);
const void* test_constvoidptr(void);
int test_constvoidptr_check(void* in);

int test_intvec(int* in, int count, int size);

int test_someblob_t_1(someblob_t* b);
int test_someblob_t_2(someblob_t* b);
int test_someunion_u_1(union someunion_u* in);
int test_someunion_u_1a(union someunion_u* in);

int test_someunion_u_2(union someunion_u* out);

int test_somestruct_1(struct somestruct_s* in);
int test_somestruct_2(struct somestruct_s* out);

void* test_get_somestruct_1f_test(void);
/* We shouldn't have test_somestruct_1f func */

int testecho_intecho(int a);

int test_outint(int* out);
int test_callcallback(void* cb, int a, int b);

/* Constants */

extern int CONST_1;
extern int MINUS_CONST_1;
extern float REAL_1;
extern int CONST_2;
extern float REAL_2;

extern int size_of_someblob_t;
extern int size_of_someunion_u;
extern int size_of_somestruct_s;
extern int size_of_someopaque_t;

typedef long someint_t;

#define FLG_1 1
#define FLG_2 2

#ifdef __cplusplus
/* { */
}
#endif

#endif
