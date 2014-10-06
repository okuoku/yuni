(library (r7b-impl base)
         (export

_
;; from R7RS draft 7
* + - ...  / < <= = => > >= abs and append apply assoc assq assv begin
binary-port? boolean=? boolean? bytevector bytevector-append bytevector-copy
bytevector-copy! bytevector-length bytevector-u8-ref bytevector-u8-set!
bytevector? caar cadr call-with-current-continuation call-with-port
call-with-values call/cc car case cdar cddr cdr ceiling char->integer
char-ready? char<=?  char<? char=? char>=? char>? char? close-input-port
close-output-port close-port complex? cond cond-expand cons current-error-port
current-input-port current-output-port define define-record-type define-syntax
define-values denominator do dynamic-wind else eof-object eof-object? eq?
equal? eqv?  error error-object-irritants error-object-message error-object?
even? exact exact-integer-sqrt exact-integer? exact? expt features file-error?
floor floor-quotient floor-remainder floor/ flush-output-port for-each gcd
get-output-bytevector get-output-string guard if import include include-ci
inexact inexact? input-port-open? input-port? integer->char integer? lambda lcm
length let let* let*-values let-syntax let-values letrec letrec* letrec-syntax
list list->string list->vector list-copy list-ref list-set! list-tail list?
make-bytevector make-list make-parameter make-string make-vector map max member
memq memv min modulo negative? newline not null? number->string number? 
numerator odd? open-input-bytevector open-input-string open-output-bytevector
open-output-string or output-port-open? output-port? pair? parameterize
peek-char peek-u8 port? positive?  procedure? quasiquote quote quotient raise
raise-continuable rational? rationalize read-bytevector read-bytevector!
read-char read-error?  read-line read-string read-u8 real? remainder reverse
round set! set-car! set-cdr! square string string->list string->number
string->symbol string->utf8 string->vector string-append string-copy
string-copy! string-fill! string-for-each string-length string-map string-ref
string-set! string<=? string<? string=? string>=? string>? string? substring
symbol->string symbol=? symbol? syntax-error syntax-rules textual-port? truncate
truncate-quotient truncate-remainder truncate/ u8-ready? unless unquote
unquote-splicing utf8->string values vector vector->list vector->string
vector-append vector-copy vector-copy! vector-fill! vector-for-each
vector-length vector-map vector-ref vector-set! vector? when
with-exception-handler write-bytevector write-char write-string write-u8 zero?
   )
         (import (except (rnrs)
                         case
                         syntax-rules
                         error
                         define-record-type
                         ;; SRFI-1
                         map for-each member assoc

                         vector-map
                         )
                 (rnrs mutable-pairs)
                 (rnrs mutable-strings)
                 (rnrs r5rs)
                 (r7b-util char-ready)
                 (r7b-util u8-ready)
                 (r7b-util port-open)
                 (r7b-util features)
                 (for (r7b-util syntax-rules) run expand)
                 (for (r7b-util case) run expand)
                 (r7b-compat i0)
                 (r7b-compat i1)
                 (r7b-compat i6)
                 (r7b-compat i6bv)
                 (r7b-compat i23)
                 (r7b-compat i9)
                 (r7b-compat i39)
                 (for (r7b-compat define-values) run expand)
                 )

;; R7RS-bridge format doesn't allow (begin (import ...) ...)
(define-syntax import
  (lambda (x)
    (syntax-case x ()
      ((_ ...) (assertion-violation 'import
                                    "Not allowed here..")))))
(define-syntax include
  (lambda (x)
    (syntax-case x ()
      ((_ ...) (assertion-violation 'include
                                    "Not allowed here..")))))

(define-syntax include-ci
  (lambda (x)
    (syntax-case x ()
      ((_ ...) (assertion-violation 'include-ci
                                    "Not allowed here..")))))

(define-syntax library
  (lambda (x)
    (syntax-case x ()
      ((_ ...) (assertion-violation 'library
                                    "Not allowed here..")))))

(define-syntax syntax-error
  (lambda (x)
    (syntax-case x ()
      ((_ message args ...) (syntax-violation 'syntax-error
                                              #'message
                                              (quote  #'(args ...)))))))
;; R7RS error object will be mapped to R6RS condition object
(define error-object? condition?)

(define (error-object-irritants obj) 
  (and (irritants-condition? obj)
       (condition-irritants obj)))

(define (error-object-message obj)
  (and (message-condition? obj)
       (condition-message obj)))

(define (open-input-bytevector bv) (open-bytevector-input-port bv))

(define (bytevector-copy-partial bv start end)
  (let ((ret (make-bytevector (- end start))))
    (define (itr cur)
      (unless (= (+ start cur) end)
        (bytevector-u8-set! ret cur (bytevector-u8-ref bv (+ start cur)))
        (itr (+ cur 1))))
    (itr 0)
    ret))

(define (bytevector-copy-partial! from start end to at)
  (define (itr cur)
    (unless (= (+ start cur) end)
      (let ((val (bytevector-u8-ref from (+ start cur))))
        (bytevector-u8-set! to (+ at cur) val)
        (itr (+ cur 1)))))
  (itr 0))

(define (exact-integer? i) (and (integer? i) (exact? i)))

(define (list-set! l k obj) 
  (define (itr cur count)
    (if (= count k) 
      (set-car! cur obj)
      (itr (cdr cur) (+ count 1))))
  (itr l 0))

(define make-list
  (case-lambda
    ((k fil) (vector->list (make-vector k fil)))
    ((k) (make-list k 'unspecified))))

(define peek-u8 
  (case-lambda
    (() (peek-u8 (current-input-port)))
    ((port)
     (lookahead-u8 port))))

(define read-bytevector 
  (case-lambda
    ((len) (read-bytevector len (current-input-port)))
    ((len port) (get-bytevector-n port len))))

(define read-string
  (case-lambda
    ((len) (read-string len (current-input-port)))
    ((len port) (get-string-n port len))))

(define read-bytevector!
  (case-lambda
    ((bv start end)
     (read-bytevector! bv start end (current-input-port)))
    ((bv start end port)
     (get-bytevector-n! port bv start (- end start)))))

(define read-line
  (case-lambda 
    (() (read-line (current-input-port)))
    ((port) (get-line port))))

(define write-u8
  (case-lambda
    ((obj) (write-u8 obj (current-output-port)))
    ((obj port) (put-u8 port obj))))

(define read-u8
  (case-lambda
    (() (read-u8 (current-input-port)))
    ((port) (get-u8 port))))

(define write-bytevector
  (case-lambda
    ((bv port)
     (put-bytevector port bv))
    ((bv) (write-bytevector bv (current-output-port))))) 

(define write-partial-bytevector
  (case-lambda
    ((bv start end) (write-partial-bytevector bv start end 
                                              (current-output-port)))
    ((bv start end port)
     (put-bytevector port bv start (- end start)))))

(define (string->vector str) (list->vector (string->list str)))
(define (vector->string vec) (list->string (vector->list vec)))
(define (vector-copy vec) (list->vector (vector->list vec)))

(define (string-map proc . strs)
  (list->string (apply map proc (map string->list strs))))

(define (bytevector . lis)
  (u8-list->bytevector lis))
(define (bytevector-append . bvs)
  (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))
(define (vector-append . lis)
  (list->vector (apply append (map vector->list lis))))

(define file-error? i/o-error?)
(define read-error? lexical-violation?)

;; From division library

(define-syntax %define-division
  (syntax-rules ()
    ((_ fix quo rem q+r)
     (begin
       (define (quo x y)
         (exact (fix (/ x y))))
       (define (rem x y)
         (- x (* (quo x y) y)))
       (define (q+r x y)
         (let ((q (quo x y)))
           (values q
                   (- x (* q y)))))))))

(%define-division
  floor
  floor-quotient
  floor-remainder0 ;; Most implementation has native modulo
  floor/)
(define floor-remainder modulo)

(define truncate-quotient quotient)
(define truncate-remainder remainder)
(define (truncate/ x y)
  (values (truncate-quotient x y)
          (truncate-remainder x y)))

(define (square x) (* x x))

(define (%string-charlist-paste to at l)
  (if (pair? l)
    (begin
      (string-set! to at (car l))
      (%string-charlist-paste to (+ at 1) (cdr l)))
    to))

(define string-copy!
  (case-lambda
    ((to at from)
     (string-copy! to at from 0 (string-length from)))
    ((to at from start)
     (string-copy! to at from start (- (string-length from) start)))
    ((to at from start end)
     (let ((s (substring from start end)))
       (%string-charlist-paste to at (string->list s)))))) 

(define vector-copy!
  (case-lambda
    ((to at from)
     (vector-copy! to at from 0 (vector-length from)))
    ((to at from start)
     (vector-copy! to at from start (- (vector-length from) start)))
    ((to at from start end)
     (if (= start end)
       to
       (begin 
         (vector-set! to at (vector-ref from start))
         (vector-copy! to (+ at 1) from (+ start 1) end))))))

(define write-string
  (case-lambda
    ((str) (write-string str (current-output-port)))
    ((str port) (put-string port str))
    ((str port start) (write-string str port start 
                                    (- (string-length str) start)))
    ((str port start end)
     (write-string (substring str start end) port))))

(define (vector-map proc . args)
  (list->vector (apply map proc (map vector->list args))))

)
