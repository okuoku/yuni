(require 'srfi-1)
(require 'byte)

(define (nan? x) (not (= x x))) ;; FIXME:
(define (inexact x) (exact->inexact x))
(define (exact x) (inexact->exact x))

;;; bytevectors
(define bytevector bytes)
(define make-bytevector make-bytes)
(define bytevector-copy bytes-copy)

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

(define bytevector-length bytes-length)

(define bytevector-copy
  (case-lambda
    ((b) (bytes-copy b))
    ((b start) (bytevector-copy b start (bytevector-length b)))
    ((b start end) (subbytes b start end))))

(define bytevector-ref byte-ref)
(define bytevector-set! byte-set!)

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
       (let ((b (bytevector-ref from start)))
        (bytevector-set! to at b)
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
