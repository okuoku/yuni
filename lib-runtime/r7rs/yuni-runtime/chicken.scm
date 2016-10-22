(define-library (yuni-runtime chicken)
                (export library quote)
                (import (scheme base))
                (begin
                  
(define-syntax library
  (syntax-rules ()
    ((_ libname (export ...) (import ...) body ...)
     (begin body ...))))))
