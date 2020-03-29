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

;; MIT/GNU Scheme does not have NaN or Inf literal (!)
(check-equal #t (nan? +nan.0))
(check-equal #f (nan? +inf.0))
;(check-equal #t (infinite? +inf.0)) ;; (yuni scheme) lacks inifinite?

;; 2 arg logs
(check-eps 2 (log 100 10))
(check-eps 12 (log 4096 2))

(check-finish)
