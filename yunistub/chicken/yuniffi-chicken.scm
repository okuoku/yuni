(foreign-declare 
  "#include <stdint.h>"
  "#include \"../common/bootstrap.inc.c\"")

(module 
  yuniffi-chicken
  (export %%yuniffi-nccc-bootstrap
          %%yuniffi-nccc-call
          %%yuniffi-module-prefix-set!-inner
          %%yuniffi-module-prefix)
  (import scheme chicken foreign lolevel)

;;

(define %%yuniffi-module-dir #f)

(define (%%yuniffi-module-prefix)
  %%yuniffi-module-dir)

(define %%yuniffi-nccc-bootstrap 
  (foreign-value "(void*)yuniffi_bootstrap0" c-pointer))

(define (%%yuniffi-module-prefix-set!-inner str)
  (set! %%yuniffi-module-dir str))

(define (%%yuniffi-nccc-call func in in_offset in_len out out_offset out_len)
  ;(display (list 'nccc-call: in)) (newline)
  (%%%yuniffi-nccc-call
    func in in_offset in_len out out_offset out_len))

(define %%%yuniffi-nccc-call
  (foreign-safe-lambda* 
    void 
    ((c-pointer func)
     (nonnull-u8vector in)
     (int in_offset)
     (int in_len)
     (nonnull-u8vector out)
     (int out_offset)
     (int out_len)) 
    "uint64_t* in0;
     uint64_t* out0;
     yuniffi_nccc_func_t callee;
     
     callee = (yuniffi_nccc_func_t)func;
     in0 = (uint64_t*)in;
     out0 = (uint64_t*)out;

     if(0){
     //printf(\"in0 = %p\\n\", in0);
     for(int x=0; x!= in_len; x++){
             printf(\"in %02d: %lx\\n\",x+in_offset,in0[x+in_offset]);
     }
     }
     
     callee(&in0[in_offset], in_len, &out0[out_offset], out_len);

     if(0){
     //printf(\"out0 = %p\\n\", out0);
     for(int y=0; y!= out_len; y++){
             printf(\"out%02d: %lx\\n\",y+out_offset,out0[y+out_offset]);
     }
     }
     ")))

(import yuniffi-chicken)

;; Exported to yuniloader
(define %%yuniffi-module-prefix-set! %%yuniffi-module-prefix-set!-inner)
