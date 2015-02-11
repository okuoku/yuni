(library (gauche-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-module-load
           yuniffi-module-lookup)
         (import (yuni scheme)
                 (rename (yuniffi)
                         (yuniffi-nccc-call yuniffi-nccc-call/raw)
                         ) ;; see yuni/yunistub/gauche
                 )
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (yuniffi-nccc-call/raw func in in-offset in-size out out-offset out-size))

(define (yuniffi-module-load path) #t)

(define (yuniffi-module-lookup handle str)
  ;; FIXME: testing
  (yuniffi-nccc-bootstrap))
         
)
