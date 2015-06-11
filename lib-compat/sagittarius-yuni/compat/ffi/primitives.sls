(library (sagittarius-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup)
         (import (yuni scheme)
                 (yuni ffi runtime simpleloader)
                 (sagittarius ffi)
                 (only (sagittarius) load-path))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (func (address in  (* 8  in-offset)) in-size 
        (address out (* 8 out-offset)) out-size))

(define (module-load path) ;; => pointer / #f
  (let ((handle (open-shared-library path)))
   (and (not (null-pointer? handle))
        handle)))
         
(define (yuniffi-module-lookup handle str)
  (define ptr (lookup-shared-library handle str))
  (pointer->c-function ptr 'void str '(void* int void* int)))

(define (module-path) (load-path))

(define yuniffi-module-load (make-simpleloader module-path module-load))

)
