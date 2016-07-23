(c-declare 
"#include <stdint.h>
#include \"../common/bootstrap.inc.c\"")

(define (%%yuniffi-nccc-call func in in_offset in_len out out_offset out_len)
  ;(display (list 'nccc-call: in)) (newline)
  (if (not (u8vector? in))
    (error "nccc-call: Invalid object for in" in))
  (if (not (u8vector? out))
    (error "nccc-call: Invalid object for out" out))
  (%%%yuniffi-nccc-call
    func in in_offset in_len out out_offset out_len))

(define %%%yuniffi-nccc-call
  (c-lambda
    (unsigned-int64 ;; 1: func
     scheme-object  ;; 2: in
     int            ;; 3: in_offset
     int            ;; 4: in_len
     scheme-object  ;; 5: out
     int            ;; 6: out_offset
     int            ;; 7: out_len
     )
    void
    "uint64_t* in0;
     uint64_t* out0;
     yuniffi_nccc_func_t callee;
     uint64_t func = ___arg1;
     void* in = ___BODY(___arg2);
     const int in_offset = ___arg3;
     const int in_len = ___arg4;
     void* out = ___BODY(___arg5);
     const int out_offset = ___arg6;
     const int out_len = ___arg7;

     callee = (yuniffi_nccc_func_t)___arg1;
     in0 = (uint64_t*)in;
     out0 = (uint64_t*)out;

     if(0){
     printf(\"in0 = %p\\n\", in0);
     for(int x=0; x!= in_len; x++){
             printf(\"in %02d: %lx\\n\",x+in_offset,in0[x+in_offset]);
     }
     }

     callee(&in0[in_offset], in_len, &out0[out_offset], out_len);

     if(0){
     printf(\"out0 = %p\\n\", out0);
     for(int y=0; y!= out_len; y++){
             printf(\"out%02d: %lx\\n\",y+out_offset,out0[y+out_offset]);
     }
     }
     ___return;
     "))

(define %%yuniffi-nccc-bootstrap
  (c-lambda
    ()
    unsigned-int64
    "___return((uint64_t)yuniffi_bootstrap0);"))

(define %%yuniffi-fetch-u8
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    unsigned-int8
    "uint8_t* in = (uint8_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-s8
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    int8
    "int8_t* in = (int8_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-u16
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    unsigned-int16
    "uint16_t* in = (uint16_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-s16
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    int16
    "int16_t* in = (int16_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-u32
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    unsigned-int32
    "uint32_t* in = (uint32_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-s32
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    int32
    "int32_t* in = (int32_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-u64
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    unsigned-int64
    "uint64_t* in = (uint64_t *)(___arg1 + ___arg2);
     ___return(*in);"))
    
(define %%yuniffi-fetch-s64
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     )
    int64
    "int64_t* in = (int64_t *)(___arg1 + ___arg2);
     ___return(*in);"))

(define %%yuniffi-store-u8
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     unsigned-int8  ;; 3: value
     )
    void
    "uint8_t* out = (uint8_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
    
(define %%yuniffi-store-s8
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     int8           ;; 3: value
     )
    void
    "int8_t* out = (int8_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
    
(define %%yuniffi-store-u16
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     unsigned-int16 ;; 3: value
     )
    void
    "uint16_t* out = (uint16_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
    
(define %%yuniffi-store-s16
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     int16          ;; 3: value
     )
    void
    "int16_t* out = (int16_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))

(define %%yuniffi-store-u32
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     unsigned-int32 ;; 3: value
     )
    void
    "uint32_t* out = (uint32_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
    
(define %%yuniffi-store-s32
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     int32          ;; 3: value
     )
    void
    "int32_t* out = (int32_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))

(define %%yuniffi-store-u64
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     unsigned-int64 ;; 3: value
     )
    void
    "uint64_t* out = (uint64_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
    
(define %%yuniffi-store-s64
  (c-lambda
    (unsigned-int64 ;; 1: ptr
     int            ;; 2: offset
     int64          ;; 3: value
     )
    void
    "int64_t* out = (int64_t*)(___arg1 + ___arg2);
     *out = ___arg3;
     ___return;"))
     
