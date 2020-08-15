(import (yuni scheme)
        (yunitest mini)
        (yuni compat ident))

(define (expected-result)
  (case (ident-impl)
    ((digamma chez chibi-scheme racket)
     #t)
    (else
      #f)))

(define a (make-vector 0))
(define b (make-vector 0))

(check-equal (expected-result) (eq? a b))

(check-finish)
