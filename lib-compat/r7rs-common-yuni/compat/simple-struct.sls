(library (r7rs-common-yuni compat simple-struct)
         (export
           make-simple-struct
           simple-struct-name
           simple-struct-ref
           simple-struct-set!
           simple-struct?)
         (import (yuni scheme))
         
(define-record-type <yuni-simple-struct>
  (%make-simple-struct0 name object)
  simple-struct?
  (name simple-struct-name)
  (object %simple-struct-obj))

(define (simple-struct-ref obj idx)
  (vector-ref (%simple-struct-obj obj) idx))
(define (simple-struct-set! obj idx v)
  (vector-set! (%simple-struct-obj obj) idx v))

(define (make-simple-struct name len lis)
  (%make-simple-struct0
    name
    (list->vector lis)))

)
