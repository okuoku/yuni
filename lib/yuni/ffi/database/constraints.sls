(library (yuni ffi database constraints)
         (export
           constraint-type
           make-constraint-bytelength
           make-constraint-count
           make-constraint-constant
           make-constraint-zero-terminated
           constraint-target
           constraint-count
           constraint-constant)
         (import (yuni scheme)
                 (yuni core))

;; constraint object         
(define* constraint (type target))         

(define* (constraint-type (constraint))
  (~ constraint 'type))

(define (make-constraint-bytelength target)
  (make constraint
        (type 'bytelength)
        (target target)))

(define* (constraint-target (constraint))
  (~ constraint 'target))

(define* (constraint-count (constraint))
  (~ constraint 'target))

(define* (constraint-constant (constraint))
  (~ constraint 'target))

(define (make-constraint-count count)
  (make constraint
        (type 'count)
        (target count)))

(define (make-constraint-constant value)
  (make constraint
        (type 'constant)
        (target value)))

(define (make-constraint-zero-terminated)
  (make constraint
        (type 'zero-terminated)
        (target #f)))

)
