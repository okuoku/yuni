(define-library (yuni-runtime r7rs)
                (export library)
                (import (scheme base))
                (begin

(define-syntax library
  (syntax-rules ()
    ((_ libname (export ...) (import ...) body ...)
     (begin body ...))))))


(import (scheme base)
        (scheme load))

(define ARG (cdr (command-line)))

(write (list 'LOADING: (cadr ARG)))(newline)

(load (cadr ARG))
