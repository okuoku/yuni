;; Numeric
(import (yuni scheme))

(define test-counter 0)
(define success-counter 0)
(define failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display success-counter)
  (display "/")
  (display test-counter)
  (display " passed.")(newline)
  (unless (null? failed-forms)
    (newline)
    (display "Failed: ")
    (newline)
    (for-each (lambda x
                (display "    ")
                (write x)
                (newline) 
                ;; YuniVM: result convertion workaround
                #t)
              (reverse failed-forms)))
  (flush-output-port (current-output-port))
  (exit (if (null? failed-forms) 0 1)))

(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (set! test-counter (+ 1 test-counter))
       (let ((e form))
        (cond ((equal? obj e)
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons 'form failed-forms)))))))))

(define-syntax check-eps
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (set! test-counter (+ 1 test-counter))
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
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons (cons e 'form) failed-forms)))))))))

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

;; MIT/GNU Scheme does not have NaN or Inf literal (!)
;(check-equal #t (nan? +nan.0))
;(check-equal #f (nan? 0))
;(check-equal #f (nan? +inf.0))
(check-equal #t (finite? 0))
(check-equal #t (finite? 0.0))
(check-equal #t (finite? 90))
;(check-equal #t (infinite? +inf.0)) ;; (yuni scheme) lacks inifinite?

(check-eps 1.0 (exp 0))
(check-eps 20.0855369231877 (exp 3))
(check-eps 0.0 (log 1))
(check-eps 1.0 (log (exp 1)))
(check-eps 42 (log (exp 42)))
(check-eps 2 (log 100 10))
(check-eps 12 (log 4096 2))
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
;(check-eps -0.0 (atan -0.0 1.0))
(check-eps 0.785398163397448 (atan 1.0 1.0))
(check-eps 1.5707963267949 (atan 1.0 0.0))
(check-eps 2.35619449019234 (atan 1.0 -1.0))
(check-eps 3.14159265358979 (atan 0.0 -1.0))
;(check-eps -3.14159265358979 (atan -0.0 -1.0))
(check-eps -2.35619449019234 (atan -1.0 -1.0))
(check-eps -1.5707963267949 (atan -1.0 0.0))
(check-eps -0.785398163397448 (atan -1.0 1.0))

(check-eps 3 (sqrt 9))
(check-eps 1.4142135623731 (sqrt 2))


(check-mod floor/ floor-quotient floor-remainder (2 1) (5 2))
(check-mod floor/ floor-quotient floor-remainder (-3 1) (-5 2))
(check-mod floor/ floor-quotient floor-remainder (-3 -1) (5 -2))
(check-mod floor/ floor-quotient floor-remainder (2 -1) (-5 -2))
(check-mod truncate/ truncate-quotient truncate-remainder (2 1) (5 2))
(check-mod truncate/ truncate-quotient truncate-remainder (-2 -1) (-5 2))
(check-mod truncate/ truncate-quotient truncate-remainder (-2 1) (5 -2))
(check-mod truncate/ truncate-quotient truncate-remainder (2 -1) (-5 -2))

(check-finish)
