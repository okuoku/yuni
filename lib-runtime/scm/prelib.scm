(require 'define-record-type)
(require 'srfi-1)
(require 'byte)
(require 'let-values)

(define (nan? x) (not (= x x))) ;; FIXME:
(define (inexact x) (exact->inexact x))
(define (exact x) (inexact->exact x))

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

;; Core I/O
(define textual-port? port?)
(define binary-port? port?)
(define open-binary-output-file open-output-file)
(define open-binary-input-file open-input-file)

(define (%read-string/itr lis k port)
  (if (= k 0)
    (list->string (reverse lis))
    (let ((c (read-char port)))
     (if (eof-object? c)
       (list->string (reverse lis))
       (%read-string/itr (cons c lis) (- k 1) port)))))

(define read-string
  (case-lambda
    ((k) (read-string k (current-input-port)))
    ((k port)
     (if (= k 0)
       ""
       (let ((c (read-char port)))
        (if (eof-object? c)
          c
          (if (= k 0)
            ""
            (%read-string/itr (list c) (- k 1) port))))))))

(define %yuni-eof-object 
  (let* ((p (open-file "/dev/null" "r"))
         (e (read-char p)))
    (close-port p)
    e))

(define (eof-object) %yuni-eof-object)

(define (open-input-bytevector bv)
  (define pos 0)
  (define end (bytevector-length bv))
  (make-soft-port
    (vector
      (lambda (c) (error "This is an input port"))
      (lambda (s) (error "This is an input port"))
      (lambda () 'do-nothing)
      (lambda () (if (= pos end)
                   (eof-object)
                   (let ((c (integer->char (bytevector-u8-ref bv pos))))
                    (set! pos (+ pos 1))
                    c)))
      (lambda () 'do-nothing))
    "r"))

(define (open-input-string str)
  (define l (string->list str))
  (make-soft-port
    (vector
      (lambda (c) (error "This is an input port"))
      (lambda (s) (error "This is an input port"))
      (lambda () 'do-nothing)
      (lambda () (if (pair? l)
                   (let ((c (car l))
                         (d (cdr l)))
                     (set! l d)
                     c)  
                   (eof-object)))
      (lambda () 'do-nothing))
    "r"))

;; FIXME: Fake

(define %yuni-bufports '())
(define (%yuni-add-bufport! p out)
  (warn "WARNING: Using fake bufport support!")
  (set! %yuni-bufports (cons (cons p out) %yuni-bufports)))
(define (%yuni-bufport-realize! p)
  (let ((x (assq p %yuni-bufports)))
   (and x
        ((cdr x)))))

(define (open-output-string)
  (define l '())
  (define (out) 
    (let ((s (list->string l)))
     (set! l '())
     s))
  (let ((p (make-soft-port
            (vector
              (lambda (c) (set! l (cons c l)))
              (lambda (s) (set! l (append (reverse (string->list s)) l)))
              (lambda () 'do-nothing)
              (lambda () (error "This is an output port"))
              (lambda () 'do-nothing))
            "w")))
    (%yuni-add-bufport! p out)
    p))

(define (open-output-bytevector)
  (define l '())
  (define (out) 
    (let ((s (list->bytes (reverse l))))
     (set! l '())
     s))
  (let ((p (make-soft-port
            (vector
              (lambda (c) (set! l (cons (char->integer c) l)))
              (lambda (s) (set! l (append 
                                    (reverse (bytes->list (string->bytes s))) 
                                    l)))
              (lambda () 'do-nothing)
              (lambda () (error "This is an output port"))
              (lambda () 'do-nothing))
            "w")))
    (%yuni-add-bufport! p out)
    p))

(define get-output-string %yuni-bufport-realize!)
(define get-output-bytevector %yuni-bufport-realize!)

(define write-string
  (case-lambda
    ((str) (display str (current-output-port)))
    ((str port) (display str port))
    ((str port start) (write-string str port start (string-length str)))
    ((str port start end) (display (substring str start end) port))))

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
    (list->bytes (reverse cur))
    (let ((c (read-u8 port)))
     (if (eof-object? c)
       (if (null? cur)
         c
         (list->bytes (reverse cur)))
       (%read-bytevector/itr (cons c cur) (- k 1) port)))))

(define read-bytevector
  (case-lambda
    ((k)
     (read-bytevector k (current-input-port)))
    ((k port)
     (%read-bytevector/itr '() k port))))

;; letrec*
(define-macro (letrec* vals . body)
  `(let () ,@(map (lambda (e) `(define . ,e)) vals) . ,body))

;;; bytevectors
(define bytevector? array?)
(define bytevector bytes)
(define make-bytevector make-bytes)
(define bytevector-copy bytes-copy)


(define bytevector-length bytes-length)

(define bytevector-copy
  (case-lambda
    ((b) (bytes-copy b))
    ((b start) (bytevector-copy b start (bytevector-length b)))
    ((b start end) (subbytes b start end))))

(define bytevector-u8-ref byte-ref)
(define bytevector-u8-set! byte-set!)

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

(define (bytevector-append . l)
  (list->bytes (apply append (map bytes->list l))))

(define utf8->string
  (case-lambda
    ((b) (bytes->string b))
    ((b start) (utf8->string b start (bytevector-length b)))
    ((b start end) (bytes->string (bytevector-copy b start end)))))

(define string->utf8
  (case-lambda
    ((s) (string->bytes s))
    ((s start) (string->utf8 s start (string-length s)))
    ((s start end) (string->bytes (substring s start end)))))

;; override
(define string<=?/r5rs string<=?)
(define (string<=? x . q)
  (if (pair? q)
    (and (string<=?/r5rs x (car q))
         (apply string<=? q))
    #t))
