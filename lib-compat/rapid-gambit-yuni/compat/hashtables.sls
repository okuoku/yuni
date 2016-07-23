(library (rapid-gambit-yuni compat hashtables)
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
           ;string-ci-hash
           symbol-hash)
         (import (yuni scheme)
                 (rapid primitive))

(define-primitive eq?-hash 'eq?-hash)
(define-primitive equal?-hash 'equal?-hash)
(define-primitive string=?-hash 'string=?-hash)
(define-primitive table-length 'table-length)
(define-primitive table-for-each 'table-for-each)
(define-primitive make-table 'make-table)
(define-primitive table-set! 'table-set!)
(define-primitive table-ref 'table-ref)
(define-primitive string->keyword 'string->keyword)
(define-primitive table? 'table?)
(define-primitive eqv?-hash 'eqv?-hash)

(define Ksize (string->keyword "size"))
(define Ktest (string->keyword "test"))
(define Khash (string->keyword "hash"))

(define make-hashtable
  (case-lambda
    ((h e) (make-table Khash h Ktest e))
    ((h e k) (make-table Khash h Ktest e Ksize k))))

(define (make-eq-hashtable)
  (make-hashtable eq?-hash eq?))
(define (make-eqv-hashtable)
  (make-hashtable eqv?-hash eqv?))

(define hashtable? table?)

(define (hashtable-size h) (error "unimpl"))
(define hashtable-ref table-ref)
(define (hashtable-set! h obj1 obj2)
  (table-set! h obj1 obj2))
(define (hashtable-delete! h obj1)
  (table-set! h obj1))
(define (hashtable-contains? . _) (error "unimpl"))
(define (hashtable-update! . _) (error "unimpl"))
(define (hashtable-copy . _) (error "unimpl"))
(define (hashtable-clear! . _) (error "unimpl"))
(define string-hash string=?-hash)
; string-ci-hash
(define (hashtable-keys h)
  (let* ((len (table-length h))
         (v (make-vector len))
         (cnt 0))
    (table-for-each 
      (lambda (k _) 
        (vector-set! v cnt k) 
        (set! cnt (+ 1 cnt))) h)
    v))
(define hashtable-keys 0)
(define (hashtable-entries . _) (error "unimpl"))
(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))
(define equal-hash equal?-hash)
(define symbol-hash eq?-hash)
         
)
