(library (chibi-scheme-yuni compat hashtables)
         (export
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
                 (srfi 69))

(define (make-hashtable h e)
  ;; FIXME: Support parameter k
  (make-hash-table e h))

(define (make-eq-hashtable)
  (make-hashtable hash-by-identity eq?))         
(define (make-eqv-hashtable)
  (make-hashtable hash-by-identity eqv?))

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
(define (hashtable-entries . _) (error "unimpl"))
(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))
(define equal-hash hash-by-identity)
; string-hash
; string-ci-hash
(define symbol-hash hash-by-identity)
)
