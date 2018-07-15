(library (r6rs-common-yuni compat hashtables)
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

           ;; Yuni extensions
           make-integer-hashtable
           make-string-hashtable
           make-symbol-hashtable
           hashtable-for-each
           hashtable-fold
           )
         (import 
           (rnrs base)
           (rnrs control)
           (rnrs hashtables))
         
(define make-integer-hashtable make-eqv-hashtable)
(define make-symbol-hashtable make-eq-hashtable)
(define (make-string-hashtable . k?)
  (if (null? k?)
    (make-hashtable string-hash string=?)
    (make-hashtable string-hash string=? (car k?))))

(define (hashtable-for-each proc ht)
  (call-with-values 
    (lambda () (hashtable-entries ht))
    (lambda (kv ev)
      (let ((end (vector-length kv)))
       (let loop ((idx 0))
        (unless (= idx end)
          (proc (vector-ref kv idx)
                (vector-ref ev idx))
          (loop (+ idx 1))))))))

(define (hashtable-fold proc s ht)
  (call-with-values
    (lambda () (hashtable-entries ht))
    (lambda (kv ev)
      (let ((end (vector-length kv)))
       (let loop ((cur s)
                  (idx 0))
         (if (= idx end)
           cur
           (loop (proc cur 
                       (vector-ref kv idx)
                       (vector-ref ev idx))
                 idx)))))))

)
