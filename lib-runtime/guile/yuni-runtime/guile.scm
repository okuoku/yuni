;; FIXME: This is currently a pure-R6RS...
;;        We should be able to retain source information with
;;        source-properties
#!r6rs
(library (yuni-runtime guile)
         (export library)
         (import (rnrs))

(define-syntax library
  (syntax-rules ()
    ((here name export import body ...)
     (begin body ...))))
         
)
