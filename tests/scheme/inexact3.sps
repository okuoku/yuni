;; Negative zero and arithmetics

(import (yuni scheme)
        (yunitest mini))

(define (signbit x)
  ;; FIXME: This doesn't work on SCM (SIBR0013)
  (char=? #\-
          (string-ref (number->string x) 0)))

(check-equal #t (= 0.0 (abs -0.0)))
(check-equal #f (signbit (abs -0.0)))
(check-equal #t (signbit (atan -0.0 1.0)))

(check-finish)
