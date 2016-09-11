#!r6rs
(library (yuni-runtime racket)
         (export library)
         (import (rnrs))

(define-syntax library
  (syntax-rules ()
    ((here name export import body ...)
     (begin body ...))))
         
)
