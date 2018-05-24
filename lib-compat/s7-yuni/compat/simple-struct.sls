(library (s7-yuni compat simple-struct)
         (export
           make-simple-struct
           simple-struct-name
           simple-struct-ref
           simple-struct-set!
           simple-struct?)
         (import (yuni scheme))


;; simple-struct0 is defined at prelib.scm
         
(define (simple-struct-name obj) (obj 'name))
(define (simple-struct-ref obj idx) (vector-ref (obj 'v) idx))
(define (simple-struct-set! obj idx v) (vector-set! (obj 'v) idx v))

(define (make-simple-struct name len lis)
  (let ((x (make-simple-struct0))
        (v (list->vector lis)))
   (set! (x 'name) name)
   (set! (x 'v) v)
   x))

(define simple-struct? simple-struct0?)
         
)
