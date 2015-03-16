(library (guile-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup)
         (import (yuni scheme)
                 (only (guile)
                       dynamic-link
                       dynamic-func)
                 (system foreign))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (let ((inp (bytevector->pointer in (* 8 in-offset)))
        (outp (bytevector->pointer out (* 8 out-offset))))
    (func inp in-size outp out-size)))

(define (yuniffi-module-load path)
  (dynamic-link path))
         
(define (yuniffi-module-lookup handle str)
  (define p (dynamic-func str handle))
  (pointer->procedure void p `(* ,int * ,int)))
         
)
