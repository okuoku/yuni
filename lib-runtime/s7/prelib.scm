(require 'stuff.scm)
(define (yuni/gensym bogus) (gensym (symbol->string bogus)))
(define (yuni/identifier? x) (symbol? x))

(define-class simple-struct0 '()
              '((name 0)
                (v 0))
              '())

(define-macro (define-values vars expr)
  `(varlet (curlet)
          ((lambda ,vars (curlet)) ,expr)))

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

;; Bytevectors
(define (string->utf8 str) (string->byte-vector str))
(define utf8->string
  (case-lambda
    ((str) str)
    ((str start) (substring str (length str)))
    ((str start end) (substring str start end))))
(define (eof-object) #<eof>)
(define bytevector-length length)
(define (bytevector-u8-ref bv idx) (bv idx))
(define bytevector byte-vector)
(define make-bytevector make-byte-vector)
(define (bytevector-u8-set! bv i x) (set! (bv i) x))

(define (close-port p)
  (if (input-port? p)
    (close-input-port p)
    (close-output-port p)))

