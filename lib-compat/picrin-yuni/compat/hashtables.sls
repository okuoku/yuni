(library (picrin-yuni compat hashtables)
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
           symbol-hash
           )
         (import (yuni scheme)
                 (picrin base))

         
(define (whoa! . x)
  (error "Actually, i'm not a hash function..."))

(define-record-type yuni-fakehashtable
  (fakehashtable mode dict)
  fakehashtable?
  (mode fakehashtable-mode)
  (dict fakehashtable-dict))

(define (make-hashtable h e)
  (define dict (make-dictionary))
  (cond
    ((eqv? e string=?)
     (fakehashtable 0 dict))
    ((eqv? e eq?)
     (fakehashtable 1 dict) )
    (else
      (error "Unknown eqv proc. for fakehashtable" e))))

(define (make-eq-hashtable)
  (make-hashtable "bogus" eq?))

(define hashtable? fakehashtable?)

(define-syntax dispatch
  (syntax-rules ()
    ((_ proc/sym proc/str h arg ...)
     (let ((mode (fakehashtable-mode h))
           (dict (fakehashtable-dict h)))
      (case mode
        ((0) (proc/str dict arg ...))
        ((1) (proc/sym dict arg ...))
        (else (error "fakehashtable dispatch: unknown mode" mode)))))))

(define ref/str
  (case-lambda
    ((dict obj val)
     (let ((r (dictionary-ref dict (string->symbol obj))))
      (if r
        (cdr r)
        val)))
    ((dict obj)
     (let ((r (dictionary-ref dict (string->symbol obj))))
      (unless r
        (error "ref/str: Not found" obj))
      (cdr r)))))

(define ref/sym
  (case-lambda
    ((dict obj val)
     (let ((r (dictionary-ref dict obj)))
      (if r
        (cdr r)
        val)))
    ((dict obj)
     (let ((r (dictionary-ref dict obj)))
      (unless r
        (error "ref/sym: Not found" obj))
      (cdr r)))) )

(define hashtable-ref
  (case-lambda
    ((h obj val)
     (dispatch ref/sym ref/str h obj val))
    ((h obj)
     (dispatch ref/sym ref/str h obj))))

(define (set!/str dict obj1 obj2)
  (dictionary-set! dict (string->symbol obj1) obj2))
(define (set!/sym dict obj1 obj2)
  (dictionary-set! dict obj1 obj2))

(define (hashtable-set! h obj1 obj2)
  (dispatch set!/sym set!/str h obj1 obj2))

(define %undefined (begin))

(define (delete!/str dict obj)
  (dictionary-set! dict (string->symbol obj) %undefined))

(define (delete!/sym dict obj)
  (dictionary-set! dict obj %undefined))

(define (hashtable-delete! h obj1)
  (dispatch delete!/sym delete!/str h obj1))

(define (keys/str dict)
  (list->vector
    (map (lambda (p) (symbol->string (car p)))
         (dictionary->alist dict))))

(define (keys/sym dict)
  (list->vector
    (map (lambda (p) (car p))
         (dictionary->alist dict))))

(define (hashtable-keys h) 
  (dispatch keys/sym keys/str h))

(define (hashtable-size h) (error "unimpl"))
(define (make-eqv-hashtable) (error "unimpl"))
(define (hashtable-contains? . _) (error "unimpl"))
(define (hashtable-update! . _) (error "unimpl"))
(define (hashtable-copy . _) (error "unimpl"))
(define (hashtable-clear! . _) (error "unimpl"))
(define (hashtable-entries . _) (error "unimpl"))
(define (hashtable-equivalence-function . _) (error "unimpl"))
(define (hashtable-hash-function . _) (error "unimpl"))
(define (hashtable-mutable? . _) (error "unimpl"))

(define equal-hash whoa!)
(define string-hash whoa!)
; string-ci-hash
(define symbol-hash whoa!)
)
