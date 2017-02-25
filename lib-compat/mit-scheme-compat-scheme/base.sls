(library (mit-scheme-compat-scheme base)
         (export
           * +
           - ...
           / <
           <= =
           => >
           >=
           _
           abs and
           append apply
           assoc assq
           assv begin
           binary-port? boolean=?
           boolean? bytevector
           bytevector-append bytevector-copy
           bytevector-copy! bytevector-length
           bytevector-u8-ref bytevector-u8-set!
           bytevector? caar
           cadr
           call-with-current-continuation
           call-with-port call-with-values
           call/cc car
           case cdar
           cddr cdr
           ceiling char->integer
           char-ready? char<=?
           char<? char=?
           char>=? char>?
           char? close-input-port
           close-output-port close-port
           complex? cond
           cond-expand cons
           current-error-port current-input-port
           current-output-port define
           define-record-type 
           define-syntax
           define-values denominator
           do dynamic-wind
           else eof-object
           eof-object? eq?
           equal? eqv?
           error error-object-irritants
           error-object-message error-object?
           even? exact
           exact-integer-sqrt exact-integer?
           exact? expt
           features file-error?
           floor floor-quotient
           floor-remainder floor/
           flush-output-port for-each
           gcd get-output-bytevector
           get-output-string guard
           if include
           include-ci inexact
           inexact? input-port-open?
           input-port? integer->char
           integer? lambda
           lcm length
           let let*
           let*-values let-syntax
           let-values letrec
           letrec* letrec-syntax
           list list->string
           list->vector list-copy
           list-ref list-set!
           list-tail list?
           make-bytevector make-list
           make-parameter make-string
           make-vector map
           max member
           memq memv
           min modulo
           negative? newline
           not null?
           number->string number?
           numerator odd?
           open-input-bytevector open-input-string
           open-output-bytevector open-output-string
           or output-port-open?
           output-port? pair?

           parameterize peek-char
           peek-u8 port?
           positive? procedure?
           quasiquote quote
           quotient raise
           raise-continuable rational?
           rationalize read-bytevector
           read-bytevector! read-char
           read-error? read-line
           read-string read-u8
           real? remainder
           reverse round
           set! set-car!
           set-cdr! square
           string string->list
           string->number string->symbol
           string->utf8 string->vector
           string-append string-copy
           string-copy! string-fill!
           string-for-each string-length
           string-map string-ref
           string-set! string<=?
           string<? string=?
           string>=? string>?
           string? substring
           symbol->string symbol=?
           symbol? syntax-error
           syntax-rules textual-port?
           truncate truncate-quotient
           truncate-remainder truncate/
           u8-ready? unless
           unquote unquote-splicing
           utf8->string values
           vector vector->list
           vector->string vector-append
           vector-copy vector-copy!
           vector-fill! vector-for-each
           vector-length vector-map
           vector-ref vector-set!
           vector? when
           with-exception-handler write-bytevector
           write-char write-string
           write-u8
           zero?

           )
         (import (yunifake-util scheme-syntax)
                 (yunifake-util define-values)
                 (scheme case-lambda))
;;

;; FIXME: current-error-port is in yuniloader..!

(define %%my-eof-object
  (let ((p (open-input-string "")))
   (read p)))

(define (eof-object) %%my-eof-object)
(define (flush-output-port p) 'do-nothing)


#|
;; Bytevectors
;; FIXME: Using strings for now..
(define bytevector? string?)
(define (bytevector . b*) 
  (list->string (map (lambda (b) (integer->char b)) b*)))
(define bytevector-append string-append)
(define bytevector-copy string-copy)
(define bytevector-length string-length)
(define bytevector-u8-ref vector-8b-ref)
(define bytevector-u8-set! vector-8b-set!)
(define make-bytevector
  (case-lambda
    ((k) (make-string k))
    ((k b) (make-string k (integer->char b)))))

(define bytevector-copy!
  (case-lambda
    ((to at from)
     (bytevector-copy!  to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (- flen start))
             (tmaxcopysize (- tlen at)))
         (bytevector-copy! to at from start (+ start
                                               (min fmaxcopysize
                                                    tmaxcopysize))))))
    ((to at from start end)
     ;; FIXME: OK??
     (substring-move-left! from start end to at))))

(define (%utf8->string u8) u8)

(define utf8->string
  (case-lambda
    ((u8) (%utf8->string u8))
    ((u8 start) (%utf8->string (substring u8 start (bytevector-length u8))))
    ((u8 start end) (%utf8->string (substring u8 start end)))))

(define (%string->utf8 str) str)

(define string->utf8
  (case-lambda
    ((str) (%string->utf8 str))
    ((str start) (%string->utf8 (substring str start (string-length str))))
    ((str start end) (%string->utf8 (substring str start end)))))
|#

;; define-record-type

(define-syntax define-record-type
  (syntax-rules ()
    ((_ name (ctr ctr-name ...)
        pred?
        flds
        ...)
     (begin
       (define name 
         (make-record-type 'name (%define-record-type-field-names flds ...)))
       (define (ctr . args) 
         (apply (record-constructor name '(ctr-name ...)) args))

       (define (pred? obj)  
         ((record-predicate name) obj))
       (%define-record-type-fields name flds)
       ...))))

(define-syntax %define-record-type-field-names
  (syntax-rules ()
    ((_ (name . bogus) ...)
     '(name ...))))

(define-syntax %define-record-type-fields
  (syntax-rules ()
    ((_ type (field-name accessor))
     (define (accessor obj)
       ((record-accessor type 'field-name) obj)))
    ((_ type (field-name accessor setter))
     (begin
       (define (setter obj v) ((record-modifier type 'field-name) obj v))
       (%define-record-type-fields pred? (field-name accessor))))))

(define-syntax when
  (syntax-rules ()
    ((_ pred code ...)
     (cond
       (pred code ...)))))

(define-syntax unless
  (syntax-rules ()
    ((_ pred code ...)
     (cond
       ((not pred)
        code ...)))))

;; Aux
(define-syntax-names/yunifake
  ...
  => 
  else 
  unquote unquote-splicing
  _)

;; Syntax
(define-syntax-names/yunifake
  and
  begin
  case 
  cond
  define
  ;define-record-type 
  define-syntax
  ;define-values 
  do 
  if 
  include
  include-ci 
  lambda
  let let*
  let*-values let-syntax
  let-values letrec
  letrec* letrec-syntax
  or 
  quasiquote quote
  set! 
  syntax-error
  syntax-rules 
  ;unless
  ;when
  )

;; Procedures
(define-primitive-names/yunifake

  * +
  - 
  / <
  <= =
  >
  >=
  abs 
  append apply
  assoc assq
  assv 
  binary-port? boolean=?
  boolean? 
  ;bytevector
  ;bytevector-append bytevector-copy
  bytevector-copy! 
  ;bytevector-length
  ;bytevector-u8-ref bytevector-u8-set!
  ;bytevector? 
  caar
  cadr
  call-with-current-continuation
  call-with-port call-with-values
  call/cc car
  cdar
  cddr cdr
  ceiling char->integer
  char-ready? char<=?
  char<? char=?
  char>=? char>?
  char? close-input-port
  close-output-port close-port
  complex? 
  cond-expand cons
  ;current-error-port 
  current-input-port
  current-output-port 
  denominator
  dynamic-wind
  ;eof-object
  eof-object? eq?
  equal? eqv?
  error error-object-irritants
  error-object-message error-object?
  even? exact
  exact-integer-sqrt exact-integer?
  exact? expt
  features file-error?
  floor floor-quotient
  floor-remainder floor/
  ;flush-output-port 
  for-each
  gcd get-output-bytevector
  get-output-string guard
  inexact
  inexact? input-port-open?
  input-port? integer->char
  integer? 
  lcm length
  list list->string
  list->vector list-copy
  list-ref list-set!
  list-tail list?
  ;make-bytevector 
  make-list
  make-parameter make-string
  make-vector map
  max member
  memq memv
  min modulo
  negative? newline
  not null?
  number->string number?
  numerator odd?
  open-input-bytevector open-input-string
  open-output-bytevector open-output-string
  output-port-open?
  output-port? pair?
  parameterize peek-char
  peek-u8 port?
  positive? procedure?
  quotient raise
  raise-continuable rational?
  rationalize read-bytevector
  read-bytevector! read-char
  read-error? read-line
  read-string read-u8
  real? remainder
  reverse round
  set-car!
  set-cdr! square
  string string->list
  string->number string->symbol
  ;string->utf8 
  string->vector
  string-append string-copy
  string-copy! string-fill!
  string-for-each string-length
  string-map string-ref
  string-set! string<=?
  string<? string=?
  string>=? string>?
  string? substring
  symbol->string symbol=?
  symbol? 
  textual-port?
  truncate truncate-quotient
  truncate-remainder truncate/
  u8-ready? 
  ;utf8->string
  values
  vector vector->list
  vector->string vector-append
  vector-copy vector-copy!
  vector-fill! vector-for-each
  vector-length vector-map
  vector-ref vector-set!
  vector? 
  with-exception-handler write-bytevector
  write-char write-string
  write-u8
  zero?
  )
         
)
