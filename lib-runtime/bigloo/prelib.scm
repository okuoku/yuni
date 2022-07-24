(define (nan? x) (not (= x x))) ;; FIXME:
(define (inexact x) (exact->inexact x))
(define (exact x) (inexact->exact x))

(define (finite? x)
  (or (exact? x)
      (finitefl? x)))

(define (infinite? x)
  (infinitefl? x))

(define (floor-remainder x y)
  ;; is, modulo
  (- x (* (exact (floor (/ x y))) y)))

(define (floor-quotient a b) 
  (exact (floor (/ a b))))

(define (floor/ a b)
  (values (floor-quotient a b)
          (floor-remainder a b)))

(define (truncate-quotient x y)
  (exact (truncate (/ x y))))

(define (truncate-remainder x y)
  ;; is, remainder
  (- x (* (truncate-quotient x y) y)))

(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))

;; case-lambda (took from r7rs.scm)
(define-macro (case-lambda . choices)
  `(lambda args
     (case (length args)
       ,@(map (lambda (choice)
                (if (or (symbol? (car choice))
                        (negative? (length (car choice))))
                  `(else (apply (lambda ,(car choice) ,@(cdr choice)) args))
                  `((,(length (car choice)))
                    (apply (lambda ,(car choice) ,@(cdr choice)) args))))
              choices))))

(define-syntax let-values
  (syntax-rules ()
    ((_ ((binds prod) cls ...) body ...)
     (receive binds prod
       (let-values (cls ...) body ...)))
    ((_ () body ...)
     (let ()
      body ...))))

(define textual-port? port?)

(define (close-port p)
  (when (input-port? p)
    (close-input-port p))
  (when (output-port? p)
    (close-output-port p)))

(define open-output-bytevector open-output-string)
(define get-output-bytevector get-output-string)

(define open-input-bytevector open-input-string)

(define open-binary-output-file open-output-file)
(define open-binary-input-file open-input-file)

(define (%write-bytevector/itr bv port start end)
  (unless (= start end)
    (write-u8 (bytevector-u8-ref bv start) port)
    (%write-bytevector/itr bv port (+ start 1) end)))

(define write-bytevector
  (case-lambda
    ((bv) (write-bytevector bv (current-output-port) 0 (bytevector-length bv)))
    ((bv port) (write-bytevector bv port 0 (bytevector-length bv)))
    ((bv port start) (write-bytevector bv port start (bytevector-length bv)))
    ((bv port start end) (%write-bytevector/itr bv port start end))))

(define write-u8
  (case-lambda
    ((b)
     (write-u8 b (current-output-port)))
    ((b port)
     (write-char (integer->char b) port))))

(define read-u8
  (case-lambda
    (()
     (read-u8 (current-input-port)))
    ((port)
     (let ((c (read-char port)))
      (if (eof-object? c)
        c
        (char->integer c))))))

(define (%read-bytevector!/itr bv port start end pos)
  (if (= start end)
    pos
    (let ((c (read-u8 port)))
     (if (eof-object? c)
       pos
       (begin
         (bytevector-u8-set! bv start c)
         (%read-bytevector!/itr bv port (+ start 1) end (+ pos 1))))))) 

(define read-bytevector!
  (case-lambda
    ((bv)
     (read-bytevector! bv (current-input-port)))
    ((bv port)
     (read-bytevector! bv port 0 (bytevector-length bv)))
    ((bv port start)
     (read-bytevector! bv port start (bytevector-length bv)))
    ((bv port start end)
     (%read-bytevector!/itr bv port start end 0))))

(define (%read-bytevector/itr cur k port)
  (if (= k 0)
    (list->string (reverse cur))
    (let ((c (read-char port)))
     (if (eof-object? c)
       (if (null? cur)
         c
         (list->string (reverse cur)))
       (%read-bytevector/itr (cons c cur) (- k 1) port)))))

(define read-bytevector
  (case-lambda
    ((k)
     (read-bytevector k (current-input-port)))
    ((k port)
     (%read-bytevector/itr '() k port))))

;;; bytevectors
(define bytevector? string?)
(define (bytevector . x) 
  (apply string
         (map integer->char x)))
(define (make-bytevector k . b) 
  (if (null? b)
    (make-string k)
    (let ((c (car b)))
     (make-string k (integer->char c)))))
(define bytevector-copy string-copy)

(define bytevector-length string-length)

(define bytevector-copy
  (case-lambda
    ((b) (string-copy b))
    ((b start) (bytevector-copy b start (bytevector-length b)))
    ((b start end) (substring b start end))))

(define (bytevector-u8-ref bv i) 
  (char->integer (string-ref bv i)))

(define (bytevector-u8-set! bv i b) 
  (string-set! bv i (integer->char b)))

(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (- flen start))
             (tmaxcopysize (- tlen at)))
         (bytevector-copy! to at from start (+ start
                                               (min fmaxcopysize
                                                    tmaxcopysize))))))
    ((to at from start end)
     (unless (= start end)
       (let ((b (bytevector-u8-ref from start)))
        (bytevector-u8-set! to at b)
        (bytevector-copy!
          to
          (+ at 1)
          from
          (+ start 1)
          end))))))

(define bytevector-append string-append)

(define utf8->string
  (case-lambda
    ((b) b)
    ((b start) (substring b start (string-length b)))
    ((b start end) (substring b start end))))

(define string->utf8 utf8->string)

;; (yuni hashtable) would conflict with Bigloo stdlib
;; So it needs to be defined here...

(define (make-integer-hashtable)
  (make-hashtable
    1024 ;; size
    65535 ;; Large enough?
    =
    get-hashnumber
    #f
    #f))

(define (make-string-hashtable)
  (make-hashtable
    1024 ;; size
    65535 ;; Large enough?
    string=?
    string-hash
    #f
    #f))

(define (make-symbol-hashtable)
  (make-hashtable
    1024 ;; size
    65535 ;; Large enough?
    eq?
    get-hashnumber
    #f
    #f))

(define (make-eq-hashtable)
  (make-hashtable
    1024 ;; size
    65535 ;; Large enough?
    eq?
    get-hashnumber
    #f
    #f))

(define (make-eqv-hashtable)
  (make-hashtable
    1024 ;; size
    65535 ;; Large enough?
    eqv?
    get-hashnumber
    #f
    #f))

(define (hashtable-ref ht x . def?)
  (if (null? def?)
    (begin
      (unless (hashtable-contains? ht x)
        (error 'hashtable-ref "Not found" x))
      (hashtable-get ht x))
    (or (hashtable-get ht x)
        (car def?)) ))

(define hashtable-set! hashtable-put!)

(define (hashtable-fold) (error "UNIMPL"))

(define (hashtable-keys ht)
  (list->vector (hashtable-key-list ht)))

(define (hashtable-entries ht)
  (let* ((size (hashtable-size ht))
         (k (make-vector size))
         (v (make-vector size))
         (cnt 0))
    (hashtable-for-each ht
                        (lambda (key value)
                          (vector-set! k cnt key)
                          (vector-set! v cnt value)
                          (set! cnt (+ 1 cnt))) )
    (values k v)))
