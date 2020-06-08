(import (yuni scheme)
        (yunitest mini))

;; R7RS
(define (sqrt0 x)
  (case x
    ((4) 2)
    ((9) 3)
    ((16) 4)
    (else #f)))

(check-equal '#(10 5 2 4 3 8) `#(10 5 ,(sqrt0 4) ,@(map sqrt0 '(16 9)) 8)) ;; S7

(check-finish)
