(library (gambit-yuni compat hashtables)
         (export
           ;; Yuni extension
           ; Constructors
           make-integer-hashtable
           make-string-hashtable
           make-symbol-hashtable
           
           hashtable-for-each
           hashtable-fold
           
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
         (import (yuni scheme))

(define-primitive-names/yunifake
  eq?-hash
  equal?-hash
  string=?-hash
  table-length
  table-for-each
  make-table
  table-set
  table-ref
  string->keyword
  table?
  eqv?-hash)

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
(define make-integer-hashtable make-eqv-hashtable)
(define (make-string-hashtable)
  (make-hashtable string=?-hash string=?))
(define make-symbol-hashtable make-eq-hashtable)

(define (hashtable-for-each . _) (error "unimpl"))
(define (hashtable-fold . _) (error "unimpl"))

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
(define (hashtable-entries h)
  (let* ((len (table-length h))
         (k (make-vector len))
         (v (make-vector len))
         (cnt 0))
    (table-for-each
      (lambda (kk vv)
        (vector-set! k cnt kk)
        (vector-set! v cnt vv)
        (set! cnt (+ 1 cnt)))
      h)
    (values k v)))

(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))
(define equal-hash equal?-hash)
(define symbol-hash eq?-hash)
         
)
