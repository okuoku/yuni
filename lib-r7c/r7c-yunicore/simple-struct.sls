(library (r7c-yunicore simple-struct)
         (export
           make-simple-struct
           simple-struct?
           simple-struct-ref
           simple-struct-set!
           simple-struct-name)
         (import (r7c-basic syntax define)
                 (r7c-ext simple-struct)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap pair))


(define (make-simple-struct/inititr! cur len ss queue)
  (unless (null? queue)
    (simple-struct-set! ss cur (car queue))
    (make-simple-struct/inititr! ($fx+ cur 1) len ss (cdr queue))))
         
(define (make-simple-struct name len lis)
  (let ((ss ($make-simple-struct name len)))
   (make-simple-struct/inititr! 0 ($fx-length lis) ss lis)
   ss))
         
)
