(library (sagittarius-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup)
         (import (yuni scheme)
                 (sagittarius ffi))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (display "FIXME: Sagittarius do not support bytevector offset protocol!\n")
  (func in in-size out out-size)
  )

(define (yuniffi-module-load path)
  (open-shared-library path))
         
(define (yuniffi-module-lookup handle str)
  (define sym (string->symbol str))
  (c-function handle void sym (void* int void* int)))
         
)
