/*
 * yuniffi.c
 */

#include "yuniffi.h"

#include "../common/bootstrap.inc.c"

#include <stdint.h>

ScmObj 
yuniffi_nccc_call(ScmObj func,
                  ScmObj in, ScmObj in_offset, ScmObj in_count,
                  ScmObj out, ScmObj out_offset, ScmObj out_count){
    yuniffi_nccc_func_t callee;
    uint64_t* in0;
    uint64_t* out0;
    int inoffs;
    int outoffs;
    int inlen;
    int outlen;

    /* Parameter check */

    if(!SCM_U8VECTORP(func)){
        Scm_Error("func: must be a u8vector", func);
        goto end;
    }

    if(!SCM_INTP(in_offset)){
        Scm_Error("in_offset: must be a fixnum", in_offset);
        goto end;
    }
    if(!SCM_INTP(in_count)){
        Scm_Error("in_count: must be a fixnum", in_count);
        goto end;
    }

    if(!SCM_INTP(out_offset)){
        Scm_Error("out_offset: must be a fixnum", out_offset);
        goto end;
    }
    if(!SCM_INTP(out_count)){
        Scm_Error("out_count: must be a fixnum", out_count);
        goto end;
    }
    if(!SCM_U8VECTORP(in)){
        Scm_Error("in: must be a u8vector", in);
        goto end;
    }
    if(!SCM_U8VECTORP(out)){
        Scm_Error("out: must be a u8vector", out);
        goto end;
    }

    callee = (yuniffi_nccc_func_t)
        (uintptr_t)
        (uint64_t)
        (*(uint64_t*)SCM_U8VECTOR_ELEMENTS(func));
    inoffs = SCM_INT_VALUE(in_offset);
    inlen = SCM_INT_VALUE(in_count);
    outoffs = SCM_INT_VALUE(out_offset);
    outlen = SCM_INT_VALUE(out_count);

    /* NB: It should be safe but we might better to use U64 vector.. */
    in0 = (uint64_t*)SCM_U8VECTOR_ELEMENTS(in);
    out0 = (uint64_t*)SCM_U8VECTOR_ELEMENTS(out);

    callee(&in0[inoffs], inlen, &out0[outoffs], outlen);

end:
    return SCM_UNDEFINED;
}

ScmObj
yuniffi_nccc_bootstrap(void){
    uint64_t ptr = (uint64_t)(uintptr_t)test0_print_and_fill;
    return Scm_MakeU8VectorFromArray(sizeof(uint64_t), (void*)&ptr);
}

/* ... left as-is */


/*
 * Module initialization function.
 */
extern void Scm_Init_yuniffilib(ScmModule*);

void Scm_Init_yuniffi(void)
{
    ScmModule *mod;

    /* Register this DSO to Gauche */
    SCM_INIT_EXTENSION(yuniffi);

    /* Create the module if it doesn't exist yet. */
    mod = SCM_MODULE(SCM_FIND_MODULE("yuniffi", TRUE));

    /* Register stub-generated procedures */
    Scm_Init_yuniffilib(mod);
}
