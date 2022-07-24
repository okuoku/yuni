;; MIT/GNU Scheme prelib
;; FIXME: Override
(define log/r5rs log)
(define (log x . y?)
  (if (null? y?)
    (log/r5rs x)
    (/ (log/r5rs x) (log/r5rs (car y?)))))
