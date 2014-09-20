(library (sagittarius-yuni compat simple-struct)
         (export
           make-simple-struct
           simple-struct-name
           simple-struct-ref
           simple-struct-set!
           simple-struct?)
         (import (rnrs)
                 (clos user))

;; FIXME: It seems something wrong with SRFI-9
;;        Use native object system for now
         
(define-class <yuni-simple-struct>
              ()
              ((name :reader simple-struct-name :init-keyword :name)
               (object :reader %simple-struct-obj :init-keyword :object)))

(define (simple-struct-ref obj idx)
  (vector-ref (%simple-struct-obj obj) idx))
(define (simple-struct-set! obj idx v)
  (vector-set! (%simple-struct-obj obj) idx v))

(define (make-simple-struct name len lis)
  (make <yuni-simple-struct>
        :name name
        :object (list->vector lis)))

)
