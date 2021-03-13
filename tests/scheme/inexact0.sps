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

(define-syntax check-mod
  (syntax-rules ()
    ((_ / quot rem (q r) (a b))
     (begin
       ;(display (list '/ 'quot 'rem '(q r) '(a b))) (newline)
       (call-with-values (lambda () (/ a b))
                         (lambda (qq rr)
                           (check-equal q qq)
                           (check-equal r rr)
                           (check-equal r (rem a b))
                           (check-equal q (quot a b))))

       (call-with-values (lambda () (/ a (inexact b)))
                         (lambda (qq rr)
                           (check-eps q qq)
                           (check-eps r rr)
                           (check-eps r (rem a (inexact b)))
                           (check-eps q (quot a (inexact b)))))))))

;; From chibi-scheme

(check-equal #f (nan? 0))
(check-equal #t (finite? 0))
(check-equal #t (finite? 0.0))
(check-equal #t (finite? 90))

(check-eps 1.0 (exp 0))
(check-eps 20.0855369231877 (exp 3))
(check-eps 0.0 (log 1))
(check-eps 1.0 (log (exp 1)))
(check-eps 42 (log (exp 42)))
(check-eps 0 (sin 0))
(check-eps 1 (sin 1.5707963267949))
(check-eps 1 (cos 0))
(check-eps -1 (cos 3.14159265358979))
(check-eps 0 (tan 0))
(check-eps 1.5574077246549 (tan 1))
(check-eps 0 (asin 0))
(check-eps 1.5707963267949 (asin 1))
(check-eps 0 (acos 1))
(check-eps 3.14159265358979 (acos -1))

(check-eps 0 (atan 0.0 1.0))
(check-eps -0.0 (atan -0.0 1.0))
(check-eps 0.785398163397448 (atan 1.0 1.0))
(check-eps 1.5707963267949 (atan 1.0 0.0))
(check-eps 2.35619449019234 (atan 1.0 -1.0))
(check-eps -2.35619449019234 (atan -1.0 -1.0))
(check-eps -1.5707963267949 (atan -1.0 0.0))
(check-eps -0.785398163397448 (atan -1.0 1.0))

(check-eps 3 (sqrt 9))
(check-eps 1.4142135623731 (sqrt 2))

;; FIXME: Add inexact x inexact = inexact cases
(check-mod floor/ floor-quotient floor-remainder (2 1) (5 2))
(check-mod floor/ floor-quotient floor-remainder (-3 1) (-5 2))
(check-mod floor/ floor-quotient floor-remainder (-3 -1) (5 -2))
(check-mod floor/ floor-quotient floor-remainder (2 -1) (-5 -2))
(check-mod truncate/ truncate-quotient truncate-remainder (2 1) (5 2))
(check-mod truncate/ truncate-quotient truncate-remainder (-2 -1) (-5 2))
(check-mod truncate/ truncate-quotient truncate-remainder (-2 1) (5 -2))
(check-mod truncate/ truncate-quotient truncate-remainder (2 -1) (-5 -2))

(check-equal #t (= 0 0))
(check-equal #t (= 123 123))
(check-equal #t (= 0 0.0 0.0))
(check-equal #t (< 1 2))
(check-equal #t (< 0 1.2 3.4))
(check-equal #f (> 1 2))
(check-equal #f (> 0 1.2 3.4))
(check-equal #f (>= 1 2))
(check-equal #f (>= 0 1.2 3.4))

(check-equal 1 (+ 1))
(check-equal 3 (+ 1 1 1))
(check-equal 0 (+))
(check-equal 1 (*))
(check-equal 0 (* 1 123 0))
(check-equal #t (number? 123))
(check-equal #f (number? number?))
(check-equal #t (integer? 123))
(check-equal #f (integer? (/ 1 3)))
(check-equal #t (inexact? 9.1))
(check-equal #f (exact? 0.1))

(check-equal 0 (exact 0.0))
(check-equal 0.0 (inexact 0))

(check-equal #t (zero? 0))
(check-equal #f (zero? 1))
(check-equal #t (positive? 1))
(check-equal #t (positive? 1.3))
(check-equal #f (positive? 0))
(check-equal #f (positive? -1))
(check-equal #f (positive? -1.9))
(check-equal #t (negative? -3))
(check-equal #t (negative? -3.559))
(check-equal #f (negative? 0))
(check-equal #f (negative? 4.1))
(check-equal #t (odd? 9))
(check-equal #t (odd? 3))
(check-equal #f (odd? 0))
(check-equal #t (even? 2))
(check-equal #t (even? 0))
(check-equal #f (even? 7))

(check-equal 123 (max 4 123))
(check-equal 123 (max 123 4))
;(check-equal 2.0 (max 0.1 1.3 1.9 2)) ;; S7

(check-equal -123 (min -123 0 890 1000))
;(check-equal -123.0 (min -123 0 9.1 444)) ;; S7

(check-finish)
