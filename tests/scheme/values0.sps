;; values
(import (yuni scheme)
        (yunitest mini))

;; 1 values, 1 arity
(call-with-values (lambda () (values 'ok))
                  (lambda (check)
                    (check-equal check 'ok)))

;; 1 values, N arity
(call-with-values (lambda () (values 'ok))
                  (lambda x
                    (check-equal '(ok) x)))

;; 2 values, 1.N arity
(call-with-values (lambda () (values 'ok 'ok))
                  (lambda (a . b)
                    (check-equal 'ok a)
                    (check-equal '(ok) b)))



(check-finish)
