(define-library (yuni-runtime picrin)
                (import (scheme base))
                (begin
                  
(define-syntax library
  (syntax-rules ()
    ((_ libname (export ...) (import ...) body ...)
     (begin body ...)))))
                (export library))
