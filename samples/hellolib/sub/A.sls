(library (sub A)
         (export sub-A)
         (import (yuni scheme))

(define (test)
  (display "This is `test` in library (sub A)\n")
  'sub-A)         

(define (sub-A)
  (let ((a (test)))
   (unless (eq? 'sub-A a)
     (error "something wrong (sub A)"))))         

)
