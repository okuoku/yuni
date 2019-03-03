(library (gauche-yuni compat hashtables)
         (export
           ;; Yuni extension
           make-symbol-hashtable
           ;; 13.1 Constructors
           make-eq-hashtable
           make-eqv-hashtable
           make-hashtable

           ;; 13.2 Procedures
           hashtable?
           hashtable-size
           hashtable-ref
           hashtable-set!
           hashtable-delete!
           hashtable-contains?
           hashtable-update!
           hashtable-copy
           hashtable-clear!
           hashtable-keys
           hashtable-entries

           ;; 13.3 Inspection
           hashtable-equivalence-function
           hashtable-hash-function
           hashtable-mutable?

           ;; 13.4 Hash functions
           equal-hash
           string-hash
           string-ci-hash
           symbol-hash
           )
         (import (yuni scheme)
                 (gauche base))

(define (make-hashtable h e)
  (unless (symbol? h)
    (error "FIXME: Custom hash function is not allowed here.." h))
  (case h
    ((USING-EQUAL-HASH) (make-hash-table 'equal?))
    ((USING-STRING-HASH) (make-hash-table 'string=?))
    ((USING-SYMBOL-HASH) (make-hash-table 'eq?))
    ((USING-EQV-HASH) (make-hash-table 'eqv?))
    (else (error "FIXME: Unrecognized hashtable type.." h))))

(define (make-eq-hashtable)
  (make-hashtable 'USING-SYMBOL-HASH eq?))         
(define (make-eqv-hashtable)
  (make-hashtable 'USING-EQV-HASH eqv?))

(define (make-symbol-hashtable) (make-eq-hashtable))

(define hashtable? hash-table?)

(define (hashtable-size h) (error "unimpl"))
(define (hashtable-ref h obj val)
  (hash-table-get h obj val))
(define (hashtable-set! h obj1 obj2)
  (hash-table-put! h obj1 obj2))
(define (hashtable-delete! h obj1)
  (hash-table-delete! h obj1))
(define (hashtable-contains? . _) (error "unimpl"))
(define (hashtable-update! . _) (error "unimpl"))
(define (hashtable-copy . _) (error "unimpl"))
(define (hashtable-clear! . _) (error "unimpl"))
(define (hashtable-keys h) 
  (list->vector (hash-table-keys h)))
(define (hashtable-entries h)
  (values
    (list->vector (hash-table-keys h))
    (list->vector (hash-table-values h))))
(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))
(define equal-hash 'USING-EQUAL-HASH)
(define string-hash 'USING-STRING-HASH)
; string-ci-hash
(define symbol-hash 'USING-SYMBOL-HASH)
)
