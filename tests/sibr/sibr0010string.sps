(import (yuni scheme)
        (yunitest mini)
        (yuni compat ident))

(define (expected-result)
  (case (ident-impl)
    ((digamma chez)
     #t)
    (else
      #f)))

(define a (make-string 0))
(define b (make-string 0))

(check-equal (expected-result) (eq? a b))

(check-finish)
