(library (sagittarius-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup
                 yuniffi-module-path)
         (import (yuni scheme)
                 (sagittarius ffi)
                 (only (sagittarius) load-path))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (func (address in  (* 8  in-offset)) in-size 
        (address out (* 8 out-offset)) out-size))

(define (yuniffi-module-load path)
  (open-shared-library path))
         
(define (yuniffi-module-lookup handle str)
  (define ptr (lookup-shared-library handle str))
  (pointer->c-function ptr 'void str '(void* int void* int)))

(define (yuniffi-module-path) (load-path))

)
