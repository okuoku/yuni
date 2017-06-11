(library (yunivm heap fake coreops)
         (export
           make-coreops-fake
           )
         (import (yuni scheme))

         
;;

(define tag-null (list 'null))
(define tag-eof-object (list 'eof-object))
(define tag-boolean (list 'boolean))
(define tag-char (list 'char))
(define tag-bignum (list 'bignum)) ;; FIXME: implement me
(define tag-flonum (list 'flonum)) ;; FIXME: implement me
(define tag-string (list 'string))
(define tag-bytevector (list 'bytevector))
(define tag-symbol (list 'symbol))
(define tag-pair (list 'pair))
(define tag-vector (list 'vector))
;; yuni extension
(define tag-unspecified (list 'unspecified))
(define tag-undefined (list 'undefined))
(define tag-simple-struct (list 'simple-struct))

;; OBJECT
(define (object? obj)
  ;; Best guess
  (and (vector? obj) 
       (pair? (vector-ref obj 0))
       (symbol? (car (vector-ref obj 0)))))

(define (wrap-object/zone0 tag)
  (vector tag))
(define (wrap-object/zone1 tag fixnum)
  (vector tag fixnum))
(define (wrap-object tag obj)
  (vector tag obj))
(define (object-tag obj)
  (vector-ref obj 0))
(define (object-datum obj)
  (vector-ref obj 1))


;; null
(define %fake-null-obj (wrap-object/zone0 tag-null))
(define (fake-null) %fake-null-obj)
(define (fake-null? obj)
  (and (object? obj)
       (eq? tag-null (object-tag obj))))

;; eof-object
(define %fake-eof-obj (wrap-object/zone0 tag-eof-object))
(define (fake-eof-object) %fake-eof-obj)
(define (fake-eof-object? obj)
  (and (object? obj)
       (eq? tag-eof-object (object-tag obj))))

;; boolean
(define %fake-true-obj (wrap-object/zone1 tag-boolean 1))
(define %fake-false-obj (wrap-object/zone1 tag-boolean 0))
(define (fake-true) %fake-true-obj)
(define (fake-false) %fake-false-obj)
(define (fake-boolean? obj)
  (and (object? obj)
       (eq? tag-boolean (object-tag obj))))
(define (fake-boolean=?/2 a b)
  (unless (and (fake-boolean? a) (fake-boolean? b))
    (error "Boolean required"))
  (let ((va (object-datum a))
        (vb (object-datum b)))
    (= va vb)))
(define (fake-true? obj)
  (and (fake-boolean? obj) (= 1 (object-datum obj))))
(define (fake-false? obj)
  (and (fake-boolean? obj) (= 0 (object-datum obj))))

;; char
(define (fake-char? obj)
  (and (object? obj)
       (eq? tag-char (object-tag obj))))
(define (fake-char=?/2 a b)
  (unless (and (fake-char? a) (fake-char? b))
    (error "Char required"))
  (let ((va (object-datum a))
        (vb (object-datum b)))
    (= va vb)))
(define (fake-integer->char int)
  (wrap-object/zone1 tag-char (%fixnum int)))
(define (fake-char->integer obj)
  (unless (fake-char? obj)
    (error "Char required"))
  (object-datum obj))

;; number = bignum flonum fixnum
(define (%fixnum host)
  ;; Ensure specified object is a fixnum
  (cond
    ((object? host)
     (let ((tag (object-tag host))
           (datum (object-datum host)))
       (cond
         ((eq? tag tag-bignum) datum)
         ((eq? tag tag-flonum) (exact datum))
         (else
           (error "Number required" tag datum)))))
    ((number? host) (exact host))
    (else (error "Number required" host))))

;; string
(define (fake-string? obj)
  (and (object? obj)
       (eq? tag-string (object-tag obj))))
(define (fake-string-length obj)
  (unless (fake-string? obj)
    (error "String required"))
  (string-length (object-datum obj)))
(define (fake-string-ref obj idx)
  (unless (fake-string? obj)
    (error "String required"))
  (let ((str (object-datum obj)))
   (fake-integer->char 
     (char->integer (string-ref str (%fixnum idx))))))
(define (fake-string-set! obj idx c)
  (unless (fake-string? obj)
    (error "String required"))
  (unless (fake-char? c)
    (error "Char required"))
  (let ((str (object-datum obj)))
   (string-set! str (%fixnum idx)
                (integer->char (object-datum c)))))
(define (fake-make-string0 count)
  (wrap-object tag-string
               (make-string (%fixnum count))))

;; bytevector
(define (fake-bytevector? obj)
  (and (object? obj)
       (eq? tag-bytevector (object-tag obj))))
(define (fake-bytevector-length obj)
  (unless (fake-bytevector? obj)
    (error "Bytevector required"))
  (bytevector-length (object-datum obj))) 
(define (fake-bytevector-u8-ref obj idx)
  (unless (fake-bytevector? obj)
    (error "Bytevector required" obj)) 
  (bytevector-u8-ref (object-datum obj) (%fixnum idx)))
(define (fake-bytevector-u8-set! obj idx n)
  (unless (fake-bytevector? obj)
    (error "Bytevector required"))
  (bytevector-u8-set! (object-datum obj)
                      (%fixnum idx) (%fixnum n)))
(define (fake-make-bytevector0 count)
  (wrap-object tag-bytevector
               (make-bytevector (%fixnum count))))

;; symbol
(define (fake-symbol? obj)
  (and (object? obj)
       (eq? tag-symbol (object-tag obj))))
(define (fake-symbol=?/2 a b)
  (unless (and (fake-symbol? a)
               (fake-symbol? b))
    (error "Symbol required"))
  (symbol=? (object-datum a)
            (object-datum b)))
(define (fake-string->symbol str)
  (unless (fake-string? str)
    (error "String required"))
  (wrap-object tag-symbol
               (string->symbol
                 (object-datum str))))
(define (fake-symbol->string sym)
  (unless (fake-symbol? sym)
    (error "Symbol required"))
  (wrap-object tag-string
               (symbol->string
                 (object-datum sym))))

;; pair
(define (fake-pair? obj)
  (and (object? obj)
       (eq? tag-pair (object-tag obj))))
(define (fake-cons a b)
  (wrap-object tag-pair (cons a b)))
(define (fake-car obj)
  (unless (fake-pair? obj)
    (error "Pair required"))
  (car (object-datum obj)))
(define (fake-cdr obj)
  (unless (fake-pair? obj)
    (error "Pair required"))
  (cdr (object-datum obj)))
(define (fake-set-car! obj a)
  (unless (fake-pair? obj)
    (error "Pair required"))
  (set-car! (object-datum obj) a))
(define (fake-set-cdr! obj d)
  (unless (fake-pair? obj)
    (error "Pair required"))
  (set-cdr! (object-datum obj) d))

;; vector
(define (fake-vector? obj)
  (and (object? obj)
       (eq? tag-vector (object-tag obj))))
(define (fake-vector-length obj)
  (unless (fake-vector? obj)
    (error "Vector required"))
  (vector-length (object-datum obj)))
(define (fake-vector-ref obj idx)
  (unless (fake-vector? obj)
    (error "Vector required"))
  (vector-ref (object-datum obj)
              (%fixnum idx)))
(define (fake-vector-set! obj idx x)
  (unless (fake-vector? obj)
    (error "Vector required"))
  (vector-set! (object-datum obj)
               (%fixnum idx)
               x))
(define (fake-make-vector0 count)
  (wrap-object tag-vector (make-vector (%fixnum count))))

;; yuni extension

;; undefined
(define %fake-undefined-obj (wrap-object/zone0 tag-undefined))
(define (fake-undefined) %fake-undefined-obj)

;; unspecified
(define %fake-unspecified-obj (wrap-object/zone0 tag-unspecified))
(define (fake-unspecified) %fake-unspecified-obj)

;; simple-struct
(define (fake-simple-struct? obj)
  (and (object? obj)
       (eq? tag-simple-struct (object-tag obj))))
(define (fake-make-simple-struct count)
  (wrap-object tag-simple-struct (make-vector (%fixnum count))))
(define (fake-simple-struct-ref obj idx)
  (unless (fake-simple-struct? obj)
    (error "Simple-struct required" obj))
  (vector-ref (object-datum obj) (%fixnum idx)))
(define (fake-simple-struct-set! obj idx x)
  (unless (fake-simple-struct? obj)
    (error "Simple-struct required"))
  (vector-set! (object-datum obj) (%fixnum idx) x))
         
         
(define (make-coreops-fake)

  (define-syntax define-query0
    (syntax-rules ()
      ((_ q (sym0 sym1) ...)
       (define (q symbol)
         (case symbol
           ((sym0) sym1)
           ...
           (else
             (error "Unknown symbol" symbol)))))))

  (define-syntax define-query1
    (syntax-rules ()
      ((_ q (cur ...) (s1 s2 s3 ...))
       (define-query1 q (cur ... (s1 s2)) (s3 ...)))
      ((_ q (cur ...) ())
       (define-query0 q cur ...))))

  (define-syntax define-query
    (syntax-rules ()
      ((_ q . rest)
       (define-query1 q () rest))))

  (define (query sym)
    (case sym
      ((null)                fake-null)
      ((null?)               fake-null?)

      ((eof-object)          fake-eof-object)
      ((eof-object?)         fake-eof-object?)

      ((true)                fake-true)
      ((false)               fake-false)
      ((boolean?)            fake-boolean?)
      ((boolean=?/2)         fake-boolean=?/2)
      ((true?)               fake-true?)
      ((false?)              fake-false?)


      ((char?)               fake-char?)
      ((char=?/2)            fake-char=?/2)
      ((integer->char)       fake-integer->char)
      ((char->integer)       fake-char->integer)

      ((string?)             fake-string?)
      ((string-length)       fake-string-length)
      ((string-ref)          fake-string-ref)
      ((string-set!)         fake-string-set!)
      ((make-string0)        fake-make-string0)

      ((bytevector?)         fake-bytevector?)
      ((bytevector-length)   fake-bytevector-length)
      ((bytevector-u8-ref)   fake-bytevector-u8-ref)
      ((bytevector-u8-set!)  fake-bytevector-u8-set!)
      ((make-bytevector0)    fake-make-bytevector0)

      ((symbol?)             fake-symbol?)
      ((symbol=?/2)          fake-symbol=?/2)
      ((string->symbol)      fake-string->symbol)
      ((symbol->string)      fake-symbol->string)

      ((pair?)               fake-pair?)
      ((cons)                fake-cons)
      ((car)                 fake-car)
      ((cdr)                 fake-cdr)
      ((set-car!)            fake-set-car!)
      ((set-cdr!)            fake-set-cdr!)

      ((vector?)             fake-vector?)
      ((vector-length)       fake-vector-length)
      ((vector-ref)          fake-vector-ref)
      ((vector-set!)         fake-vector-set!)
      ((make-vector0)        fake-make-vector0)

      ((undefined)           fake-undefined)
      ((unspecified)         fake-unspecified)

      ((simple-struct?)      fake-simple-struct?)
      ((make-simple-struct)  fake-make-simple-struct)
      ((simple-struct-ref)   fake-simple-struct-ref)
      ((simple-struct-set!)  fake-simple-struct-set!)
      (else (error "Unknown symbol" sym))))

  query)
)

