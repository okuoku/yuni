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
(define %yuni-eof-object 
  (let* ((p (open-file "/dev/null" "r"))
         (e (read-char p)))
    (close-port p)
    e))

(define (eof-object) %yuni-eof-object)

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

(define write-string
  (case-lambda
    ((str) (display str (current-output-port)))
    ((str port) (display str port))
    ((str port start) (write-string str port start (string-length str)))
    ((str port start end) (display (substring str start end) port))))

;; letrec*
(define-macro (letrec* vals . body)
  `(let () ,@(map (lambda (e) `(define . ,e)) vals) . ,body))

;;; bytevectors
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

(define string<=?/r5rs string<=?)
(define (string<=? x . q)
  (if (pair? q)
    (and (string<=?/r5rs x (car q))
         (apply string<=? q))
    #t))
