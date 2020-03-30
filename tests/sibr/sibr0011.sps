(import (yuni scheme)
        (yuni compat ident)
        (yunitest mini))

(define (expected-result)
  (case (ident-impl)
    ((racket) #f)
    (else #t)))

(check-equal (expected-result) (file-exists? "."))

(check-finish)
