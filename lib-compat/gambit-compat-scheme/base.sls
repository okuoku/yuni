(library (gambit-compat-scheme base)
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
         (import 
           (yunifake-util scheme-syntax)
           (yunifake-util define-values)
           (scheme case-lambda))
;;
(define %%my-eof-object
  (let ((p (open-input-string "")))
   (read p)))

(define (eof-object) %%my-eof-object)
(define (flush-output-port p) 'do-nothing)


;; Bytevectors
(define bytevector? u8vector?)
(define bytevector u8vector)
(define bytevector-append u8vector-append)
(define bytevector-copy u8vector-copy)
(define bytevector-length u8vector-length)
(define bytevector-u8-ref u8vector-ref)
(define bytevector-u8-set! u8vector-set!)
(define make-bytevector make-u8vector)

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
     (subu8vector-move! from start end to at))))

(define (%utf8->string u8) ;; FIXME: Detect illegal sequence more strictly
  (define len (u8vector-length u8))
  (define (complain) (error "Illegal utf8 sequence" u8))
  (call-with-output-string
    '()
    (lambda (p)
      (let loop ((idx 0))
       (define rest (- len idx))
       (cond
         ((= idx len)
          ;; DONE
          p)
         (else
           (let ((c0 (u8vector-ref u8 idx)))
            (cond
              ((< c0 #x80) ;; 1 byte
               (write-char (integer->char c0) p)
               (loop (+ 1 idx)))
              ((< c0 #xe0) ;; 2 bytes
               (if (< rest 1) (complain))
               (let* ((c1 (u8vector-ref u8 (+ 1 idx)))
                      (i (bitwise-ior
                           (arithmetic-shift (bitwise-and #x1f c0) 6)
                           (bitwise-and #x3f c1))))
                 (write-char (integer->char i) p))
               (loop (+ 2 idx)))
              ((< c0 #xf0) ;; 3 bytes
               (if (< rest 2) (complain))
               (let* ((c1 (u8vector-ref u8 (+ 1 idx)))
                      (c2 (u8vector-ref u8 (+ 2 idx)))
                      (i (bitwise-ior
                           (arithmetic-shift (bitwise-and #xf c0) 12)
                           (arithmetic-shift (bitwise-and #x3f c1) 6)
                           (bitwise-and #x3f c2))))
                 (write-char (integer->char i) p))
               (loop (+ 3 idx)))
              ((< c0 #xf5) ;; 4 bytes
               (if (< rest 3) (complain))
               (let* ((c1 (u8vector-ref u8 (+ 1 idx)))
                      (c2 (u8vector-ref u8 (+ 2 idx)))
                      (c3 (u8vector-ref u8 (+ 3 idx)))
                      (i (bitwise-ior
                           (arithmetic-shift (bitwise-and #x7 c0) 18)
                           (arithmetic-shift (bitwise-and #x3f c1) 12)
                           (arithmetic-shift (bitwise-and #x3f c2) 6)
                           c3)))
                 (write-char (integer->char i) p))
               (loop (+ 4 idx)))
              (else (complain))))))))))

(define utf8->string
  (case-lambda
    ((u8) (%utf8->string u8))
    ((u8 start) (%utf8->string (subu8vector u8 start (u8vector-length u8))))
    ((u8 start end) (%utf8->string (subu8vector u8 start end)))))

(define (%string->utf8 str)
  (define len (string-length str))
  (call-with-output-u8vector
    '()
    (lambda (p)
      (let loop ((idx 0))
       (cond
         ((= idx len)
          ;; DONE
          p)
         (else
           (let ((i (char->integer (string-ref str idx))))
            (cond
              ((< i #x80) ;; 1 byte
               (write-u8 i p))
              ((< i #x800) ;; 2 bytes
               (let ((c0 (bitwise-ior #xc0 (bitwise-and
                                             #x1f (arithmetic-shift i -6))))
                     (c1 (bitwise-ior #x80 (bitwise-and #x3f i))))
                 (write-u8 c0 p)
                 (write-u8 c1 p)))
              ((< i #x10000) ;; 3 bytes
               (let ((c0 (bitwise-ior #xe0 (bitwise-and
                                             #xf (arithmetic-shift i -12))))
                     (c1 (bitwise-ior #x80 (bitwise-and
                                             #x3f (arithmetic-shift i -6))))
                     (c2 (bitwise-ior #x80 (bitwise-and #x3f i))))
                 (write-u8 c0 p)
                 (write-u8 c1 p)
                 (write-u8 c2 p)))
              ((< i #x110000) ;; 4 bytes
               (let ((c0 (bitwise-ior #xf0 (bitwise-and
                                             #x7 (arithmetic-shift i -18))))
                     (c1 (bitwise-ior #x80 (bitwise-and
                                             #x3f (arithmetic-shift i -12))))
                     (c2 (bitwise-ior #x80 (bitwise-and
                                             #x3f (arithmetic-shift i -6))))
                     (c3 (bitwise-ior #x80 (bitwise-and #x3f i))))
                 (write-u8 c0 p)
                 (write-u8 c1 p)
                 (write-u8 c2 p)
                 (write-u8 c3 p)))
              (else
                (error "Invalid character" i str))))
           (loop (+ 1 idx))))))))

(define string->utf8
  (case-lambda
    ((str) (%string->utf8 str))
    ((str start) (%string->utf8 (substring str start (string-length str))))
    ((str start end) (%string->utf8 (substring str start end)))))

;; define-record-type

(define-syntax define-record-type
  (syntax-rules ()
    ((_ name (ctr ctr-name ...)
        pred?
        flds
        ...)
     (begin
       (define name (list 'table-type))
       (define (ctr ctr-name ...)
         (define theRecord (make-table))
         (table-set! theRecord '%%yunifake-record-type name)
         (table-set! theRecord 'ctr-name ctr-name)
         ...
         theRecord)
       (define (pred? obj)
         (and (table? obj)
              (eq? name (table-ref obj '%%yunifake-record-type))))

       (%define-record-type-fields pred? flds)
       ...))))

(define-syntax %define-record-type-fields
  (syntax-rules ()
    ((_ pred? (field-name accessor))
     (begin
       (define (accessor obj)
         (cond
           ((pred? obj)
            (table-ref obj 'field-name))
           (else
             (error "Unexpected object" pred? obj))))))
    ((_ pred? (field-name accessor setter))
     (begin
       (define (setter obj val)
         (cond
           ((pred? obj)
            (table-set! obj 'field-name val))
           (else
             (error "Unexpected object" pred? obj))))
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
  current-error-port current-input-port
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
