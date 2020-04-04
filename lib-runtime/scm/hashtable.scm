(require 'hash-table)

(define-record-type <yuniht>
  (%make-yuniht ht eq hash setter getter remover) %yuniht?
  (ht %yuniht-ht)
  (eq %yuniht-eq)
  (hash %yuniht-hash)
  (setter %yuniht-setter)
  (getter %yuniht-getter)
  (remover %yuniht-remover))

(define %yuniht-makehashtable
  (case-lambda
    ((eqv) (%yuniht-makehashtable (predicate->hash eqv) eqv))
    ((h e)
     (%make-yuniht
       (make-hash-table #x1001) ;; FIXME: some prime here
       e
       h
       (hash-associator e)
       (hash-inquirer e)
       (hash-remover e)))))

(define (make-eq-hashtable)
  (%yuniht-makehashtable eq?))
(define (make-eqv-hashtable)
  (%yuniht-makehashtable eqv?))
(define (make-integer-hashtable)
  (%yuniht-makehashtable =))
(define (make-string-hashtable)
  (%yuniht-makehashtable string=?))
(define (make-hashtable h e)
  (%yuniht-makehashtable h e))

(define make-symbol-hashtable make-eq-hashtable)

(define (hashtable? obj) (%yuniht? obj))

(define (hashtable-size h) (error "unimpl"))

(define hashtable-ref
  (case-lambda
    ((h obj val)
     ;; FIXME: Properly detect #f entry
     (let ((x ((%yuniht-getter h) (%yuniht-ht h) obj)))
      (or x
          val)))
    ((h obj)
     (let ((x ((%yuniht-getter h) (%yuniht-ht h) obj)))
      (unless x
        (error "not found"))
      x))))

(define (hashtable-set! h k obj)
  ((%yuniht-setter h) (%yuniht-ht h) k obj))

(define (hashtable-delete! h k)
  ((%yuniht-remover h) (%yuniht-ht h) k))

(define (hashtable-keys h)
  (define k* '())
  (vector-for-each 
    (lambda (a) (set! k* (append a k*)))
    (%yuniht-ht h))
   (list->vector (map (lambda (e) (car e)) k*)))

(define (hashtable-entries h)
  (define k* '())
  (vector-for-each 
    (lambda (a) (set! k* (append a k*)))
    (%yuniht-ht h))
  (values
    (list->vector (map (lambda (e) (car e)) k*)) 
    (list->vector (map (lambda (e) (cdr e)) k*))))

(define (hashtable-for-each ht proc)
  (vector-for-each
    (lambda (a)
      (for-each (lambda (e) (proc (car e) (cdr e))) a))
    (%yuniht-ht ht)))

(define (hashtable-fold . x) (error "unimpl"))
(define (hashtable-update! . x) (error "unimpl"))
