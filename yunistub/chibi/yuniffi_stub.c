#include <stdint.h>
#include <chibi/eval.h>

#include "../common/bootstrap.inc.c"

void
yuniffi_nccc_call(void* func /* Cpointer */,
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

void*
yuniffi_nccc_bootstrap(void){
    void* ptr = (void*)yuniffi_bootstrap0;
    return ptr;
}

int
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
                    mag = data[0] + (data[1] << 32);
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

void* 
yuniffi_offset_ptr(void* ptr, sexp offset){
    void* ret;
    ret = ptr + (intptr_t)sexp_to64(offset);
}

int 
yuniffi_fetch_s8(void* ptr, int offset){
    signed char c;
    void *x = ptr + offset;
    c = *(signed char*)x;
    return c;
}

int
yuniffi_fetch_u8(void* ptr, int offset){
    unsigned char c;
    void *x = ptr + offset;
    c = *(unsigned char*)x;
    return c;
}

int
yuniffi_fetch_s16(void* ptr, int offset){
    int16_t c;
    void *x = ptr + offset;
    c = *(int16_t*)x;
    return c;
}

int
yuniffi_fetch_u16(void* ptr, int offset){
    uint16_t c;
    void *x = ptr + offset;
    c = *(uint16_t*)x;
    return c;
}

int
yuniffi_fetch_s32(void* ptr, int offset){
    int32_t c;
    void *x = ptr + offset;
    c = *(int32_t*)x;
    return c;
}

unsigned int
yuniffi_fetch_u32(void* ptr, int offset){
    uint32_t c;
    void *x = ptr + offset;
    c = *(uint32_t*)x;
    return c;
}

void 
yuniffi_store_s8(void* ptr, int offset, int value){
    int8_t* x = (ptr + offset);
    *x = value;
}
void 
yuniffi_store_u8(void* ptr, int offset, unsigned int value){
    uint8_t* x = (ptr + offset);
    *x = value;
}
void
yuniffi_store_s16(void* ptr, int offset, int value){
    int16_t* x = (ptr + offset);
    *x = value;
}
void
yuniffi_store_u16(void* ptr, int offset, unsigned int value){
    uint16_t* x = (ptr + offset);
    *x = value;
}
void
yuniffi_store_s32(void* ptr, int offset, int value){
    int32_t* x = (ptr + offset);
    *x = value;
}
void
yuniffi_store_u32(void* ptr, int offset, unsigned int value){
    uint32_t* x = (ptr + offset);
    *x = value;
}

