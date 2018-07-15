(library (chez-r7b-util bytevector-buffer)
         (export get-output-bytevector
                 open-output-bytevector)
         (import (chezscheme))
         
(define (open-output-bytevector)
  (call-with-values 
    (lambda () (open-bytevector-output-port))
    (lambda (p proc) p)))         

(define (get-output-bytevector p)
  ;; NB: Depends on implementation details
  (let ((cur (binary-port-output-buffer p))
        (curlen (port-length p)))
    (let ((out (bytevector-truncate! (bytevector-copy cur) curlen)))
     (set-port-length! p 0)
     out)))
         
)
