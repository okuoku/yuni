(define-library (yuni-runtime r7rs)
                (export library)
                (import (scheme base))
                (begin
                  
(define-syntax library
  (syntax-rules ()
    ((_ libname (export ...) (import ...) body ...)
     (begin body ...))))))
