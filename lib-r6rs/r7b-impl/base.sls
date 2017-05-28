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
         (import (rename
                   (except (rnrs)
                         case
                         syntax-rules
                         error
                         define-record-type
                         ;; SRFI-1
                         map for-each member assoc

                         vector-map)
                   (vector-fill! r6:vector-fill!)
                   (string->list r6:string->list)
                   (vector->list r6:vector->list)
                   (string-copy r6:string-copy)
                   (bytevector-copy r6:bytevector-copy)
                   (bytevector-copy! r6:bytevector-copy!)
                   (utf8->string r6:utf8->string)
                   (string->utf8 r6:string->utf8))
                 (rnrs mutable-pairs)
                 (rename (rnrs mutable-strings)
                         (string-fill! r6:string-fill!))
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
                                              (syntax message)
                                              (quote  (syntax (args ...))))))))
;; R7RS error object will be mapped to R6RS condition object
(define error-object? condition?)
(define file-error? i/o-error?)
(define read-error? lexical-violation?)

(define (error-object-irritants obj) 
  (and (irritants-condition? obj)
       (condition-irritants obj)))

(define (error-object-message obj)
  (and (message-condition? obj)
       (condition-message obj)))

;; Ports
(define (open-input-bytevector bv) (open-bytevector-input-port bv))

(define (exact-integer? i) (and (integer? i) (exact? i)))

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
    ((bv)
     (read-bytevector! bv (current-input-port)))
    ((bv port)
     (read-bytevector! bv port 0))
    ((bv port start)
     (read-bytevector! bv port start (bytevector-length bv)))
    ((bv port start end)
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
    ((bv) (write-bytevector bv (current-output-port))) 
    ((bv port) (put-bytevector port bv))
    ((bv port start) (write-bytevector (%subbytevector1 bv start) port))
    ((bv port start end)
     (write-bytevector (%subbytevector bv start end) port))))

(define write-string
  (case-lambda
    ((str) (write-string str (current-output-port)))
    ((str port) (put-string port str))
    ((str port start) (write-string str port start 
                                    (- (string-length str) start)))
    ((str port start end)
     (write-string (substring str start end) port))))


;; List additions
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

;; Vector and string additions
;; FIXME: Optmize them
(define (string-map proc . strs)
  (list->string (apply map proc (map r6:string->list strs))))

(define (vector-map proc . args)
  (list->vector (apply map proc (map r6:vector->list args))))

(define (bytevector . lis)
  (u8-list->bytevector lis))
(define (bytevector-append . bvs)
  (u8-list->bytevector (apply append (map bytevector->u8-list bvs))))
(define (vector-append . lis)
  (list->vector (apply append (map r6:vector->list lis))))

;; Substring functionalities added
;;; string
(define (%substring1 str start) (substring str start (string-length str)))

(define string->list
  (case-lambda
    ((str) (r6:string->list str))
    ((str start) (r6:string->list (%substring1 str start)))
    ((str start end) (r6:string->list (substring str start end)))))

(define string->vector
  (case-lambda
    ((str) (list->vector (string->list str)))
    ((str start) (string->vector (%substring1 str start)))
    ((str start end) (string->vector (substring str start end)))))

(define string-copy
  (case-lambda
    ((str) (r6:string-copy str))
    ((str start) (%substring1 str start))
    ((str start end) (substring str start end))))

(define string->utf8
  (case-lambda
    ((str) (r6:string->utf8 str))
    ((str start) (r6:string->utf8 (%substring1 str start)))
    ((str start end) (r6:string->utf8 (substring str start end)))))

(define string-fill!
  (case-lambda
    ((str fill) (r6:string-fill! str fill))
    ((str fill start) (string-fill! str fill start (string-length str)))
    ((str fill start end)
     (define (itr r)
       (unless (= r end)
         (string-set! str r fill)
         (itr (+ r 1))))
     (itr start))))

;;; vector
(define (%subvector v start end)
  (define mlen (- end start))
  (define out (make-vector (- end start)))
  (define (itr r)
    (if (= r mlen)
      out
      (begin
        (vector-set! out r (vector-ref v (+ start r)))
        (itr (+ r 1)))))
  (itr 0))

(define (%subvector1 v start) (%subvector v start (vector-length v)))

(define vector-copy
  (case-lambda
    ((v) (%subvector1 v 0))
    ((v start) (%subvector1 v start))
    ((v start end) (%subvector v start end))))

(define vector->list
  (case-lambda
    ((v) (r6:vector->list v))
    ((v start) (r6:vector->list (%subvector1 v start)))
    ((v start end) (r6:vector->list (%subvector v start end)))))

(define vector->string
  (case-lambda
    ((v) (list->string (vector->list v)))
    ((v start) (vector->string (%subvector1 v start)))
    ((v start end) (vector->string (%subvector v start end)))))

(define vector-fill!
  (case-lambda
    ((vec fill) (r6:vector-fill! vec fill))
    ((vec fill start) (vector-fill! vec fill start (vector-length vec)))
    ((vec fill start end)
     (define (itr r)
       (unless (= r end)
         (vector-set! vec r fill)
         (itr (+ r 1))))
     (itr start))))

(define (%subbytevector bv start end)
  (define mlen (- end start))
  (define out (make-bytevector mlen))
  (r6:bytevector-copy! bv start out 0 mlen)
  out)

(define (%subbytevector1 bv start)
  (%subbytevector bv start (bytevector-length bv)))

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
     (r6:bytevector-copy! from start to at (- end start)))))

(define bytevector-copy
  (case-lambda
    ((bv) (r6:bytevector-copy bv))
    ((bv start) (%subbytevector1 bv start))
    ((bv start end) (%subbytevector bv start end))))

(define utf8->string
  (case-lambda
    ((bv) (r6:utf8->string bv))
    ((bv start) (r6:utf8->string (%subbytevector1 bv start)))
    ((bv start end) (r6:utf8->string (%subbytevector bv start end)))))

(define (%string-copy!-neq to at from start end)
  (define term (+ at (- end start)))
  (define (itr r)
    (unless (= r term)
      (string-set! to r (string-ref from (+ start (- r at))))
      (itr (+ r 1))))
  (itr at))

(define string-copy!
  (case-lambda
    ((to at from)
     (string-copy! to at from 0 (string-length from)))
    ((to at from start)
     (string-copy! to at from start (string-length from)))
    ((to at from start end)
     (if (eq? to from) ;; FIXME: Handle overlap..
       (string-copy! to at (substring from start end))
       (%string-copy!-neq to at from start end)))))

(define (%vector-copy!-neq to at from start end)
  (define term (+ at (- end start)))
  (define (itr r)
    (unless (= r term)
      (vector-set! to r (vector-ref from (+ start (- r at))))
      (itr (+ r 1))))
  (itr at))

(define vector-copy!
  (case-lambda
    ((to at from)
     (vector-copy! to at from 0 (vector-length from)))
    ((to at from start)
     (vector-copy! to at from start (vector-length from)))
    ((to at from start end)
     (if (eq? to from)
       (vector-copy! to at (%subvector from start end))
       (%vector-copy!-neq to at from start end)))))

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



)
