(library (sub hello)
         (export subhello)
         (import (yuni scheme))

(define (subhello)
  (display "Hello (sub)\n")
  #t)         
         
)
