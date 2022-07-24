(import (yuni scheme)
        (yunitest mini))

(define-syntax check-eps
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (let ((e form))
        (cond ((let* ((diff (- obj e))
                      (rate
                        (cond
                          ((> 0.000001 (abs diff)) diff)
                          (else (inexact (/ (abs diff) (abs obj)))))))
                 ;; Match epsilon and sign
                 (and (> 0.0001 rate)
                      (if (negative? obj)
                        (negative? e)
                        #t)))
               (check-equal #t #t))
              (else
                (check-equal #t 'form))))))))

(unless (eqv? 0.0 -0.0)
  (check-eps 3.14159265358979 (atan 0.0 -1.0))
  (check-eps -3.14159265358979 (atan -0.0 -1.0)))

(check-finish)
