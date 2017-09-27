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

    if(!YUNIPTR_P(func)){
        Scm_Error("func: must be a pointer", func);
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
        YUNIPTR_UNBOX(func);
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
yuniffi_nccc_proc_register(ScmObj proc){
    ScmObj* box;
    if(!SCM_PROCEDUREP(proc)){
        Scm_Error("proc: must be a procedure", proc);
        return SCM_UNDEFINED;
    }

    box = GC_MALLOC_UNCOLLECTABLE(sizeof(ScmObj));
    *box = proc;

    return YUNIPTR_BOX(box);
}

ScmObj
yuniffi_nccc_proc_release(ScmObj ptr){
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    GC_FREE(YUNIPTR_UNBOX(ptr));
}

void
callback_bridge(uintptr_t procobjptr,
                uint64_t* in, int in_len,
                uint64_t* out, int out_len){
    ScmObj* box = (ScmObj*)(void*)procobjptr;
    Scm_ApplyRec4(*box, 
                  YUNIPTR_BOX(in), Scm_MakeInteger(in_len),
                  YUNIPTR_BOX(out), Scm_MakeInteger(out_len));
}

ScmObj
yuniffi_nccc_get_callback_bridge(void){
    uint64_t ptr = (uint64_t)(uintptr_t)callback_bridge;
    return YUNIPTR_BOX((void*)(uintptr_t)ptr);
}

ScmObj
yuniffi_pointer_fromint(ScmObj offset){
    if(!SCM_INTP(offset)){
        Scm_Error("offset: must be a fixnum", offset);
        return SCM_UNDEFINED;
    }
    return YUNIPTR_BOX((void*)(uintptr_t)SCM_INT_VALUE(offset));
}

static uint64_t
pointer_fetch_unsigned(void* ptr, ssize_t offset, int width){
    uint64_t r;
    void* p;
    p = ptr + offset;

    switch(width){
        case 1:
            r = *(uint8_t *)p;
            break;
        case 2:
            r = *(uint16_t *)p;
            break;
        case 4:
            r = *(uint32_t *)p;
            break;
        case 8:
            r = *(uint64_t *)p;
            break;
        default:
            Scm_Panic("Unexpected.");
    }

    return r;
}

static int64_t
pointer_fetch_signed(void* ptr, ssize_t offset, int width){
    int64_t r;
    void* p;
    p = ptr + offset;

    switch(width){
        case 1:
            r = *(int8_t *)p;
            break;
        case 2:
            r = *(int16_t *)p;
            break;
        case 4:
            r = *(int32_t *)p;
            break;
        case 8:
            r = *(int64_t *)p;
            break;
        default:
            Scm_Panic("Unexpected.");
    }

    return r;
}

static void
pointer_store(void* ptr, ssize_t offset, int width, uint64_t data){
    void* p;
    p = ptr + offset;

    switch(width){
        case 1:
            *(uint8_t *)p = data;
            break;
        case 2:
            *(uint16_t *)p = data;
            break;
        case 4:
            *(uint32_t *)p = data;
            break;
        case 8:
            *(uint64_t *)p = data;
            break;
        default:
            Scm_Panic("Unexpected.");
    }
}

ScmObj
yuniffi_pointer_fetch_signed(ScmObj ptr, ScmObj offset, ScmObj width){
    void* p;
    ssize_t offs;
    int w;
    int64_t r;
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTP(width)){
        Scm_Error("width: must be a fixnum", width);
        return SCM_UNDEFINED;
    }

    p = YUNIPTR_UNBOX(ptr);
    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    w = SCM_INT_VALUE(width);

    r = pointer_fetch_signed(p, offs, w);

    return Scm_MakeInteger64(r);
}

ScmObj
yuniffi_pointer_fetch_unsigned(ScmObj ptr, ScmObj offset, ScmObj width){
    void* p;
    ssize_t offs;
    int w;
    uint64_t r;
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTP(width)){
        Scm_Error("width: must be a fixnum", width);
        return SCM_UNDEFINED;
    }

    p = YUNIPTR_UNBOX(ptr);
    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    w = SCM_INT_VALUE(width);

    r = pointer_fetch_unsigned(p, offs, w);

    return Scm_MakeIntegerU64(r);
}

ScmObj
yuniffi_pointer_fetch_p64(ScmObj ptr, ScmObj offset){
    void* p;
    uint64_t* in;
    ssize_t offs;
    uint64_t r;
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }

    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    p = YUNIPTR_UNBOX(ptr) + offs;
    in = (uint64_t *)p;

    return YUNIPTR_BOX((void*)(uintptr_t)*in);
}

ScmObj
yuniffi_pointer_fetch_p64_bv(ScmObj bv, ScmObj offset){
    void* p;
    uint64_t* in;
    ssize_t offs;
    uint64_t r;
    if(!SCM_U8VECTORP(bv)){
        Scm_Error("bv: must be a u8vector", in);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }

    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    p = SCM_U8VECTOR_ELEMENTS(bv) + offs;
    in = (uint64_t *)p;

    return YUNIPTR_BOX((void*)(uintptr_t)*in);
}

ScmObj
yuniffi_pointer_store(ScmObj ptr, ScmObj offset, ScmObj width, ScmObj data){
    void* p;
    ssize_t offs;
    int w;
    int64_t d0s;
    uint64_t d;
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTP(width)){
        Scm_Error("width: must be a fixnum", width);
        return SCM_UNDEFINED;
    }

    p = YUNIPTR_UNBOX(ptr);
    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    w = SCM_INT_VALUE(width);
    //d0s = Scm_GetInteger64Clamp(data, SCM_CLAMP_BOTH, NULL);
    /* FIXME: Where's Scm_GetInteger64UClamp? */
    d = Scm_GetInteger64Clamp(data, SCM_CLAMP_ERROR, NULL);

    pointer_store(p, offs, w, d);

    return SCM_UNDEFINED;
}

ScmObj
yuniffi_pointer_store_p64(ScmObj ptr, ScmObj offset, ScmObj data){
    void* p;
    ssize_t offs;
    void* d;
    if(!YUNIPTR_P(ptr)){
        Scm_Error("ptr: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!YUNIPTR_P(data)){
        Scm_Error("data: must be a pointer", ptr);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }

    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    p = YUNIPTR_UNBOX(ptr) + offs;
    d = YUNIPTR_UNBOX(data);
    *((void**)p) = d;

    return SCM_UNDEFINED;
}

ScmObj
yuniffi_pointer_store_p64_bv(ScmObj bv, ScmObj offset, ScmObj data){
    void* p;
    ssize_t offs;
    void* d;
    if(!SCM_U8VECTORP(bv)){
        Scm_Error("bv: must be a u8vector", bv);
        return SCM_UNDEFINED;
    }
    if(!YUNIPTR_P(data)){
        Scm_Error("data: must be a pointer", data);
        return SCM_UNDEFINED;
    }
    if(!SCM_INTEGERP(offset)){
        Scm_Error("offset: must be a number", offset);
        return SCM_UNDEFINED;
    }

    offs = Scm_GetInteger64Clamp(offset, SCM_CLAMP_ERROR, NULL);
    p = SCM_U8VECTOR_ELEMENTS(bv) + offs;
    d = YUNIPTR_UNBOX(data);
    *((void**)p) = d;

    return SCM_UNDEFINED;
}


ScmObj
yuniffi_nccc_bootstrap(void){
    uint64_t ptr = (uint64_t)(uintptr_t)yuniffi_bootstrap0;
    return YUNIPTR_BOX((void*)(uintptr_t)ptr);
}

ScmClass *YuniPtrCls;

static void
yuniptr_print(ScmObj obj, ScmPort *out, ScmWriteContext *ctx){
    void* me = YUNIPTR_UNBOX(obj);
    Scm_Printf(out, "#<yuniptr \"%p\">", me);
}

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

    /* (Yuniffi specific begin) */

    /* Register yuniffi pointer class */
    YuniPtrCls = Scm_MakeForeignPointerClass(mod, "<yuniptr>",
                                            yuniptr_print,
                                            NULL,
                                            0);
}
