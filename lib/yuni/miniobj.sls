(library (yuni miniobj)
         (export miniobj-ref
                 miniobj-set!
                 miniobj-typeof)
         (import (yuni scheme)
                 (yuni miniobj rnrs)
                 (yuni miniobj minitype)
                 (yuni miniobj minidispatch)
                 (yuni miniobj base))

(define-miniobj-typeof miniobj-typeof
                       miniobj-minidispatch-typeof
                       miniobj-minitype-typeof
                       (lambda (_) #f))

(define-miniobj-ref miniobj-ref
                    miniobj-minidispatch-ref
                    miniobj-minitype-ref
                    miniobj-rnrs-ref-error)

(define-miniobj-set! miniobj-set!
                     miniobj-minidispatch-set!
                     miniobj-minitype-set!
                     miniobj-rnrs-set!-error)

)
