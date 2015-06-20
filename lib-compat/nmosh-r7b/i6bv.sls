(library (nmosh-r7b i6bv)
         (export
           get-output-bytevector
           open-output-bytevector)
         (import (rnrs) 
                 (yuni-nmosh primitives))

(define get-output-bytevector sys-get-bytevector)
(define (open-output-bytevector) (sys-open-bytevector-output-port))

)
