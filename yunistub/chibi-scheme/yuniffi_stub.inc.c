#include <stdint.h>
#include <stdio.h>
#include <chibi/eval.h>

#include "../common/bootstrap.inc.c"

static void
yuniffi_nccc_call(sexp ctx, void* func /* Cpointer */,
                  sexp in /* U8 */, int in_offset, int in_len,
                  sexp out /* U8 */, int out_offset, int out_len){

    uint64_t* in0;
    uint64_t* out0;
    yuniffi_nccc_func_t callee;
    
    callee = (yuniffi_nccc_func_t)func;
    in0 = (uint64_t*)sexp_bytes_data(in);
    out0 = (uint64_t*)sexp_bytes_data(out);

    callee(&in0[in_offset], in_len, &out0[out_offset], out_len);
}

static void
do_call(sexp ctx, sexp proc, 
        uint64_t* in, int in_len, uint64_t* out, int out_len){
    sexp_gc_var1(args);
    sexp_gc_preserve1(ctx, args);
    args = sexp_list2(ctx, sexp_make_cpointer(ctx, SEXP_CPOINTER,
                                              out, SEXP_FALSE, 0),
                      sexp_make_fixnum(out_len));
    args = sexp_cons(ctx, sexp_make_fixnum(in_len), args);
    args = sexp_cons(ctx, sexp_make_cpointer(ctx, SEXP_CPOINTER,
                                             in, SEXP_FALSE, 0), args);
    sexp_apply(ctx, proc, args);
    sexp_gc_release1(ctx);
}

static void
call_nccc_proc(uintptr_t procobjptr,
               uint64_t* in, int in_len,
               uint64_t* out, int out_len){
    sexp pair;
    sexp ctx;
    sexp proc;
    pair = (sexp)procobjptr;
    ctx = sexp_car(pair);
    proc = sexp_cdr(pair);
    do_call(ctx, proc, in, in_len, out, out_len);
}

static void*
yuniffi_nccc_get_callback_bridge(void){
    /* It's not ISO C compliant */
    void* ptr = (void*)call_nccc_proc;
    return ptr;
}

static void*
yuniffi_nccc_bootstrap(void){
    /* It's not ISO C compliant */
    void* ptr = (void*)yuniffi_bootstrap0;
    return ptr;
}

static int
yuniffi_pointerp(sexp obj){
    if(sexp_cpointerp(obj)){
        return 1;
    }else{
        return 0;
    }
}

static int64_t
sexp_to64(sexp val){
    uint64_t mag;
    int sign;
    sexp_uint_t* data;
    if(sexp_fixnump(val)){
        return sexp_unbox_fixnum(val);
    }else if(sexp_bignump(val)){
        if(sexp_integerp(val)){
            data = sexp_bignum_data(val);
            if(sexp_exact_negativep(val)){
                sign = -1;
            }else{
                sign = 1;
            }
            if(4 == sizeof(sexp_uint_t)){
                if(sexp_bignum_length(val) > 1){
                    mag = data[0] + (((uint64_t)data[1]) << 32);
                }else{
                    mag = data[0];
                }
            }else if(8 == sizeof(sexp_uint_t)){
                mag = data[0];
            }
            return mag * sign;
        }
    }
    printf("Unexpected type.\n");
    abort();
    return 0; /* NOTREACHED */
}

/* FIXME: Unused? */
static void* 
yuniffi_offset_ptr(void* ptr, sexp offset){
    void* ret;
    ret = ptr + (intptr_t)sexp_to64(offset);
}

static int 
yuniffi_fetch_s8(void* ptr, int offset){
    signed char c;
    void *x = ptr + offset;
    c = *(signed char*)x;
    return c;
}

static int
yuniffi_fetch_u8(void* ptr, int offset){
    unsigned char c;
    void *x = ptr + offset;
    c = *(unsigned char*)x;
    return c;
}

static int
yuniffi_fetch_s16(void* ptr, int offset){
    int16_t c;
    void *x = ptr + offset;
    c = *(int16_t*)x;
    return c;
}

static int
yuniffi_fetch_u16(void* ptr, int offset){
    uint16_t c;
    void *x = ptr + offset;
    c = *(uint16_t*)x;
    return c;
}

static int
yuniffi_fetch_s32(void* ptr, int offset){
    int32_t c;
    void *x = ptr + offset;
    c = *(int32_t*)x;
    return c;
}

static unsigned int
yuniffi_fetch_u32(void* ptr, int offset){
    uint32_t c;
    void *x = ptr + offset;
    c = *(uint32_t*)x;
    return c;
}

static void 
yuniffi_store_s8(void* ptr, int offset, int value){
    int8_t* x = (ptr + offset);
    *x = value;
}

static void 
yuniffi_store_u8(void* ptr, int offset, unsigned int value){
    uint8_t* x = (ptr + offset);
    *x = value;
}

static void
yuniffi_store_s16(void* ptr, int offset, int value){
    int16_t* x = (ptr + offset);
    *x = value;
}

static void
yuniffi_store_u16(void* ptr, int offset, unsigned int value){
    uint16_t* x = (ptr + offset);
    *x = value;
}

static void
yuniffi_store_s32(void* ptr, int offset, int value){
    int32_t* x = (ptr + offset);
    *x = value;
}
static void
yuniffi_store_u32(void* ptr, int offset, unsigned int value){
    uint32_t* x = (ptr + offset);
    *x = value;
}

static void*
yuniffi_fetch_p64(void* ptr, int offset){
    void* in = ptr + offset;
    uint64_t* in0;
    in0 = (uint64_t *)in;
    return (void*)(uintptr_t)(*in0);
}

static void
yuniffi_store_p64(void* ptr, int offset, void* value){
    void* out = ptr + offset;
    uint64_t* out0;
    out0 = (uint64_t *)out;
    *out0 = (uint64_t)(uintptr_t)value;
}

static void*
yuniffi_fetch_p64_bv(sexp bv, int offset){
    void* in;
    in = sexp_bytes_data(bv);
    return yuniffi_fetch_p64(in, offset);
}

static void
yuniffi_store_p64_bv(sexp bv, int offset, void* value){
    void* out;
    out = sexp_bytes_data(bv);
    yuniffi_store_p64(out, offset, value);
}
