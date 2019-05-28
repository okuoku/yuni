(library (s7-yuni compat hashtables)
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
           hashtable-keys
           hashtable-size)
         (import (yuni scheme))

;; s7's hash-table is different from SRFI-69
;; https://ccrma.stanford.edu/software/snd/snd/s7.html#hashtables

(define (make-integer-hashtable)
  (make-hash-table 8 =))
(define (make-string-hashtable)
  (make-hash-table 8 string=?))
(define (make-symbol-hashtable)
  (make-hash-table 8 eq?))

(define (hashtable-for-each . _) (error "unimpl"))
(define (hashtable-fold . _) (error "unimpl"))

(define (make-eq-hashtable)
  (make-hash-table 8 eq?))

(define %%yuni-false-guard-obj (cons #f #f))
(define (%%yuni-hashtable-result-filter v fallback)
  (cond
    ((eq? v %%yuni-false-guard-obj)
     #f)
    ((eq? v #f) fallback)
    (else v)))

(define (hashtable-set! ht k v)
  (if (eq? #f v)
    (hash-table-set! ht k %%yuni-false-guard-obj)
    (hash-table-set! ht k v)))

(define (hashtable-ref ht k fallback)
  (let ((v (hash-table-ref ht k)))
   (%%yuni-hashtable-result-filter v fallback)))

(define (hashtable-update! . _) (error "unimpl"))

(define (hashtable-entries ht)
  (let* ((len (hash-table-entries ht))
         (k (make-vector len))
         (v (make-vector len))
         (cnt 0))
    (for-each (lambda (p)
                (vector-set! k cnt (car p))
                (vector-set! v cnt 
                             (%%yuni-hashtable-result-filter (cdr p) #f))
                (set! cnt (+ 1 cnt)))
              ht)
    (values k v)))

(define (hashtable-size ht) (hash-table-entries ht))
(define (hashtable-keys ht)
  (let* ((len (hash-table-entries ht))
         (k (make-vector len))
         (cnt 0))
    (for-each (lambda (p)
                (vector-set! k cnt (car p))
                (set! cnt (+ 1 cnt)))
              ht)
    k))
)
