(library (chez-r7b-util bytevector-buffer)
         (export get-output-bytevector
                 open-output-bytevector)
         (import (chezscheme))

(define theStore (make-weak-eq-hashtable))
         
(define (open-output-bytevector)
  (call-with-values 
    (lambda () (open-bytevector-output-port))
    (lambda (p proc)
      (hashtable-set! theStore p proc)
      p)))         

(define (get-output-bytevector p)
  (let ((proc (hashtable-ref theStore p #f)))
   (unless proc
     (error #f "???"))
   (proc)))
         
)
