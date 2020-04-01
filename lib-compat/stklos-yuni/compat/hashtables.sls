(library (stklos-yuni compat hashtables)
         (export
           ;; Yuni extensions
           make-symbol-hashtable
           ;; 13.1 Constructors
           make-eq-hashtable
           make-eqv-hashtable
           make-integer-hashtable
           make-string-hashtable
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

           hashtable-for-each
           hashtable-fold
           )
         ;; SRFI-69
         (import (yuni scheme))

(define (make-hashtable h e)
  ;; FIXME: Support parameter k
  (make-hash-table e h))

(define (make-eq-hashtable)
  (make-hash-table eq?))
(define (make-eqv-hashtable)
  (make-hash-table eqv?))
(define (make-integer-hashtable)
  (make-hash-table =))
(define (make-string-hashtable)
  (make-hash-table string=?))

(define hashtable-for-each hash-table-for-each)
(define (hashtable-fold . arg) (error "unimpl"))

(define make-symbol-hashtable make-eq-hashtable)

(define hashtable? hash-table?)

(define (hashtable-size h) (error "unimpl"))
(define hashtable-ref
  (case-lambda
    ((h obj val)
     (hash-table-ref/default h obj val))
    ((h obj)
     (hash-table-ref h obj))))
(define (hashtable-set! h obj1 obj2)
  (hash-table-set! h obj1 obj2))
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
(define equal-hash hash-table-hash)
(define string-hash hash-table-hash)
(define string-ci-hash hash-table-hash)
(define symbol-hash hash-table-hash)
)
