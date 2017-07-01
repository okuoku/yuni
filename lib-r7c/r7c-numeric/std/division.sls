(library (r7c-numeric std division)
         (export floor/
                 floor-quotient
                 floor-remainder
                 truncate/
                 truncate-quotient
                 truncate-remainder)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap flonum))

(define (floor/ a b)
  (if ($flonum? a)
    (if ($flonum? b)
      ($fl-floor/ a b)
      ($fl-floor/ a ($fx->fl b)))
    (if ($flonum? b)
      ($fl-floor/ ($fx->fl a) b)
      ($fx-floor/ a b))))

(define (truncate/ a b)
  (if ($flonum? a)
    (if ($flonum? b)
      ($fl-truncate/ a b)
      ($fl-truncate/ a ($fx->fl b)))
    (if ($flonum? b)
      ($fl-truncate/ ($fx->fl a) b)
      ($fx-truncate/ a b))))

(define (floor-quotient a b)
  (call-with-values (lambda () (floor/ a b))
                    (lambda (q r) q)))

(define (floor-remainder a b)
  (call-with-values (lambda () (floor/ a b))
                    (lambda (q r) r)))

(define (truncate-quotient a b)
  (call-with-values (lambda () (truncate/ a b))
                    (lambda (q r) q)))

(define (truncate-remainder a b)
  (call-with-values (lambda () (truncate/ a b))
                    (lambda (q r) r)))
)
