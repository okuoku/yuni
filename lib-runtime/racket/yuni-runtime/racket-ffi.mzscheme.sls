#!r6rs
(library (yuni-runtime racket-ffi)
         (export nccc-func 
                 ptr-ref/cpointer
                 ptr-set/cpointer!)
         (import (rnrs)
                 (ffi unsafe)) 
(define nccc-func
  (_fun _gcpointer _int _gcpointer _int -> _void))

;; FIXME: Assumes little endian
(define (%calc-ptr-offset off)
  (define size (ctype-sizeof _pointer))
  (case size
    ((4)
     (bitwise-arithmetic-shift-right off 2))
    ((8)
     (bitwise-arithmetic-shift-right off 3))
    (else (assertion-violation '%calc-ptr-offset 
                               "Unknown pointer size" size))))
(define (ptr-ref/cpointer p off)
  (define o (%calc-ptr-offset off))
  (ptr-ref p _pointer o))
(define (ptr-set/cpointer! p off v)
  (define o (%calc-ptr-offset off))
  (ptr-set! p _pointer o v))

)
