(library (racket-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup
                 yuniffi-module-path)
         (import (yuni scheme)
                 ;; Some of definitions are placed in runtime
                 ;; because we need to use keywords and other Racket specific
                 ;; notations.
                 (rename
                   (only (racket base) 
                         map
                         path->string
                         current-library-collection-paths)
                   (map map:racket))
                 (yuni-runtime racket-ffi)
                 (ffi unsafe))

;;

(define (yuniffi-nccc-call func 
                           in in-offset in-size
                           out out-offset out-size) 
  ;; Bytevector is byte string in Racket. So we don't have to convert them.
  (define inp (and in (ptr-add in (* in-offset 8))))
  (define outp (and out (ptr-add out (* out-offset 8))))
  (func inp in-size outp out-size))

(define (yuniffi-module-load path)
  (ffi-lib path))


(define (yuniffi-module-lookup handle str)
  ;; nccc-func is defined in (yuni-runtime racket-ffi)
  (get-ffi-obj str handle nccc-func
               ;; Failure thunk
               (lambda () #f)))

(define (yuniffi-module-path) (map:racket path->string
                                   (current-library-collection-paths)))
         
)
