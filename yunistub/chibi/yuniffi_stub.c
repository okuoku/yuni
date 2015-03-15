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
    void* ptr = (void*)test0_print_and_fill;
    return ptr;
}
