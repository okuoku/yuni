(import (yuni scheme)
        (yuni compat ident)
        (yunitest mini))

(define (checkans s)
  (cond
    ((and (string? s) (string=? s ""))
     #f)
    ((eof-object? s)
     #t)
    (else "Unexpected response!")))


(check-equal #t (eqv? 0.0 0.0))
(check-equal #t (eqv? -0.0 -0.0))
(check-equal #f (eqv? -0.0 0.0))
(check-equal #f (eqv? 0.0 -0.0))
(check-equal #f (negative? -0.0))
(check-equal #f (positive? -0.0))
(check-equal #f (negative? 0.0))
(check-equal #f (positive? 0.0))

(check-finish)
