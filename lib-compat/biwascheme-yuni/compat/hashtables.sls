(library (biwascheme-yuni compat hashtables)
         (export
           ;; Yuni extension
           ; Constructors
           make-integer-hashtable
           make-string-hashtable
           make-symbol-hashtable
           ; Iter
           hashtable-for-each
           hashtable-fold

           ;; R6RS
           ; Constructor
           make-eq-hashtable
           ; Mutators
           hashtable-set!
           hashtable-update!
           ; Accessor
           hashtable-entries
           hashtable-ref
           ; Query
           hashtable-size)
         (import (yuni scheme))

(define (make-integer-hashtable)
  (make-eqv-hashtable))
(define (make-string-hashtable)
  (make-hashtable string-hash string=?))
(define (make-symbol-hashtable)
  (make-hashtable symbol-hash eq?))

(define (hashtable-for-each . _) (error "unimpl"))
(define (hashtable-fold . _) (error "unimpl"))

)
