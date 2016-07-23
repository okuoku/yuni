(foreign-declare 
  "#include <stdint.h>"
  "#include \"../common/bootstrap.inc.c\"")

(module 
  yuniffi-chicken
  (export %%yuniffi-nccc-bootstrap
          %%yuniffi-nccc-call)
  (import scheme chicken foreign)

;;

(define %%yuniffi-nccc-bootstrap 
  (foreign-value "(void*)yuniffi_bootstrap0" c-pointer))

(define %%yuniffi-nccc-call
  (foreign-safe-lambda* 
    void 
    ((c-pointer func)
     (blob in)
     (int in_offset)
     (int in_len)
     (blob out)
     (int out_offset)
     (int out_len)) 
    "uint64_t* in0;
     uint64_t* out0;
     yuniffi_nccc_func_t callee;
     
     callee = (yuniffi_nccc_func_t)func;
     in0 = (uint64_t*)in;
     out0 = (uint64_t*)out;
     
     callee(&in0[in_offset], in_len, &out0[out_offset], out_len);")))
