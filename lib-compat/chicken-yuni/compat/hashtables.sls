(library (chicken-yuni compat hashtables)
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
           string-ci-hash
           symbol-hash
           )
         (import (yuni scheme)
                 (srfi 69))

(define (make-hashtable h e)
  ;; FIXME: Support parameter k
  (make-hash-table e h))

(define (make-eq-hashtable)
  (make-hash-table eq? hash-by-identity))         
(define (make-eqv-hashtable)
  (make-hash-table eqv? hash-by-identity))

(define (make-integer-hashtable)
  (make-hash-table = hash))
(define (make-string-hashtable)
  (make-hash-table string=? string-hash))

(define (make-symbol-hashtable)
  (make-hash-table symbol=? symbol-hash))

(define (hashtable-for-each . _) (error "unimpl"))
(define (hashtable-fold . _) (error "unimpl"))

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
  (let* ((len (hash-table-size h))
         (k (make-vector len))
         (v (make-vector len))
         (cnt 0))
    (hash-table-walk h
                     (lambda (kk vv)
                       (vector-set! k cnt kk)
                       (vector-set! v cnt vv)
                       (set! cnt (+ 1 cnt))))
    (values k v)))

(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))
(define equal-hash hash-by-identity)
; string-hash
; string-ci-hash
(define symbol-hash hash-by-identity)
)
