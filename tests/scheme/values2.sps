;; zero values
(import (yuni scheme)
        (yunitest mini))

;; 0 values, 0 arity
(define term0 'fail)
(call-with-values (lambda () (values))
                  (lambda () 
                    (set! term0 'ok)))
(check-equal term0 'ok)

;; 0 values, N arity
(call-with-values (lambda () (values))
                  (lambda x
                    (check-equal 0 (length x))))

(check-finish)
