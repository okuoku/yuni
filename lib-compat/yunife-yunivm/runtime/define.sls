(library (yunife-yunivm runtime define)
         (export define)
         (import (yunivm-core-syntax))

(define-syntax define
  (syntax-rules ()
    ((_ name body0)
     ($define/core name body0))
    ((_ (name . args) body0 body1 ...)
     ($define/core name (lambda args body0 body1 ...)))))         
         
)
