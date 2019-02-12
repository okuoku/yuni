(library (B)
         (export B)
         (import (yuni scheme))

#|
 ;; On Generic Scheme implementations,
 ;; un-commenting following `stxB` definition
 ;; will break (test) definition, because
 ;; yuni runtime will "promote" every globals
 ;; on these impementations.
 ;; 
(define-syntax stxB
  (syntax-rules ()
    ((_ sym)
     (begin
       (display "stxB: Symbol ")
       (display 'sym)
       (display " value is: ")
       (write sym)
       (display "\n")))))
|#

(define (test)
  (display "This is `test` in library (B)\n")
  'B)         

(define (B)
  (let ((a (test)))
   (unless (eq? 'B a)
     (error "something wrong (B)"))))         

)
