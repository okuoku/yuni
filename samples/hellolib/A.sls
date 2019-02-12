(library (A)
         (export A stxA)
         (import (yuni scheme))

(define-syntax stxA
  (syntax-rules ()
    ((_ sym)
     (begin
       (display "stxA: Symbol ")
       (display 'sym)
       (display " value is: ")
       (write sym)
       (display "\n")))))

(define (test)
  (display "This is `test` in library (A)\n")
  'A)         

(define (A)
  (let ((a (test)))
   (unless (eq? 'A a)
     (error "something wrong (A)"))))         


)
