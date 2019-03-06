(library (A)
         (export A stxA)
         (import (yuni scheme)
                 (yunitest mini))

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
  (check-equal (list 'a 'b 10 "string")
               (let ((rest (list 10 "string"))
                     (c 'b))
                `(a ,c ,@rest)))
  'A)         

(define (A)
  (let ((a (test)))
   (unless (eq? 'A a)
     (error "something wrong (A)"))))         


)
