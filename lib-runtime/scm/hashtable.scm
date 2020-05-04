(require 'hash)
(require 'alist)
(require 'hash-table)

(define-record-type <yuniht>
  (%make-yuniht ht eq hash setter getter/2 getter/3 remover) %yuniht?
  (ht %yuniht-ht)
  (eq %yuniht-eq)
  (hash %yuniht-hash)
  (setter %yuniht-setter)
  (getter/2 %yuniht-getter/2)
  (getter/3 %yuniht-getter/3)
  (remover %yuniht-remover))

(define (%yuniht-makehashtable h e)
  (%make-yuniht
    (make-hash-table #x1001) ;; FIXME: some prime here
    e
    h
    (%yuniht-hash-associator h e)
    (%yuniht-hash-inquirer/2 h e)
    (%yuniht-hash-inquirer/3 h e)
    (%yuniht-hash-remover h e)))

(define (%yuniht-hash-associator h e)
  (let ((asso (alist-associator e)))
   (lambda (ht k obj)
     (let ((i (h k (vector-length ht))))
      (vector-set! ht i
                   (asso (vector-ref ht i) k obj))))))

(define (%yuni-alist-inq2 e)
  (letrec ((loop (lambda (alist key)
                   (if (pair? alist)
                     (let* ((p (car alist))
                            (a (car p))
                            (d (cdr p)))
                       (if (e a key)
                         d
                         (loop (cdr alist) key)))
                     (error "not found")))))
    loop))

(define (%yuni-alist-inq3 e)
  (letrec ((loop (lambda (alist key fallback)
                   (if (pair? alist)
                     (let* ((p (car alist))
                            (a (car p))
                            (d (cdr p)))
                       (if (e a key)
                         d
                         (loop (cdr alist) key fallback)))
                     fallback))))
    loop))

(define (%yuniht-hash-inquirer/2 h e) ;; NB: Differs from SLIB
  (let ((inquirer/2 (%yuni-alist-inq2 e)))
   (lambda (ht k)
     (inquirer/2 (vector-ref ht (h k (vector-length ht)))
                 k))))

(define (%yuniht-hash-inquirer/3 h e) ;; NB: Differs from SLIB
  (let ((inquirer/3 (%yuni-alist-inq3 e)))
   (lambda (ht k val)
     (inquirer/3 (vector-ref ht (h k (vector-length ht)))
                 k
                 val))))

(define (%yuniht-hash-remover h e)
  (let ((aremov (alist-remover e)))
   (lambda (ht k)
     (let ((i (h k (vector-length ht))))
      (vector-set! ht i
                   (aremov (vector-ref ht i) k))))))

;; SLIB `hashv` may return negative number
;;   (hashv -1 100) ;; => -2
;; Thus we need workaround

(define (make-eq-hashtable)
  (%yuniht-makehashtable hashq eq?))
(define (%yuniht-safehashv obj k)
  (let ((h (hashv obj k)))
   (if (negative? h)
     (- h)
     h)))
(define (make-eqv-hashtable)
  (%yuniht-makehashtable %yuniht-safehashv eqv?))
(define (make-integer-hashtable)
  (%yuniht-makehashtable %yuniht-safehashv =))
(define (make-string-hashtable)
  (%yuniht-makehashtable hashv string=?))
(define (make-hashtable h e)
  (%yuniht-makehashtable h e))

(define make-symbol-hashtable make-eq-hashtable)

(define (hashtable? obj) (%yuniht? obj))

(define (hashtable-size h) (error "unimpl"))

(define hashtable-ref
  (case-lambda
    ((h obj val)
     ((%yuniht-getter/3 h) (%yuniht-ht h) obj val))
    ((h obj)
     ((%yuniht-getter/2 h) (%yuniht-ht h) obj))))

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
