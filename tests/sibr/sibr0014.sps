(import (yuni scheme) 
        (yuni compat ident)
        (yunitest mini))

(define (expected-result)
  (case (ident-impl)
    ((s7) 1)
    (else 0)))

(call-with-values
  (lambda () (values))
  (lambda a (check-equal (length a) (expected-result))))

(check-finish)
