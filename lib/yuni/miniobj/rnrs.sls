(library (yuni miniobj rnrs)
         (export miniobj-rnrs-ref-error
                 miniobj-rnrs-set!-error)
         (import (yuni scheme))

(define (miniobj-rnrs-ref-error obj slot)
  (error "miniobj: unsupported object" (list obj slot)))
(define (miniobj-rnrs-set!-error obj slot value)
  (error "miniobj: unsupported object" (list obj slot value)))

)
