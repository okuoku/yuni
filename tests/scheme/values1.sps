;; let-values

(import (yuni scheme)
        (yunitest mini))

(define term0 'fail)
(let-values ()
            (set! term0 'ok))

(check-equal term0 'ok)

(let-values (((a) 'ok)
             ((b) 'ok2))
            (check-equal a 'ok)
            (check-equal b 'ok2))

(define (v2) (values 'ok0 'ok3))

(let-values (((a b) (v2)))
            (check-equal a 'ok0)
            (check-equal b 'ok3))

(check-finish)
