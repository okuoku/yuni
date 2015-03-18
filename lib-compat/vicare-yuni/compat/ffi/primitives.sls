(library (vicare-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup)
         (import (yuni scheme)
                 (only (vicare)
                       bytevector->memory
                       pointer-add
                       )
                 (vicare ffi))
;; Guile style bytevector->pointer
(define (bytevector->pointer bv offs)
  ;; FIXME: Do we have to use bytevector->guarded-memory ??
  (call-with-values (lambda () (bytevector->memory bv))
                    (lambda (ptr _) (pointer-add ptr offs))))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (let ((inp (bytevector->pointer in (* 8 in-offset)))
        (outp (bytevector->pointer out (* 8 out-offset))))
    (func inp in-size outp out-size)))

(define nccc-callout-maker
  (make-c-callout-maker 'void '(pointer signed-int pointer signed-int)))

(define (yuniffi-module-load path)
  (dlopen path))
         
(define (yuniffi-module-lookup handle str)
  (nccc-callout-maker (dlsym handle str)))
         
)
