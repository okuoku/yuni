(library (yunivm heap fake coreops)
         (export make-coreops-fake)
         (import (yuni scheme))
         
;;

(define tag-null (list 'null))
(define tag-eof-object (list 'eof-object))
(define tag-boolean (list 'boolean))
(define tag-char (list 'char))
(define tag-bignum (list 'bignum)) ;; FIXME: implement me
(define tag-flonum (list 'flonum))
(define tag-string (list 'string))
(define tag-bytevector (list 'bytevector))
(define tag-symbol (list 'symbol))
(define tag-pair (list 'pair))
(define tag-vector (list 'vector))
;; yuni extension
(define tag-unspecified (list 'unspecified))
(define tag-undefined (list 'undefined))
(define tag-simple-struct (list 'simple-struct))
(define tag-primitive (list 'primitive))
(define tag-vmclosure (list 'vmclosure))

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
#|
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
|#
(define (%fixnum host) host)

(define (fake-fixnum? obj) (and (number? obj) (integer? obj)))

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
(define (fake-string-fill! obj c start end)
  (unless (fake-string? obj)
    (error "String required"))
  (unless (fake-char? c)
    (error "Char required"))
  (string-fill! (object-datum obj) (integer->char (object-datum c))
                (%fixnum start) (%fixnum end)))

(define (fake-string-copy! to at from start end)
  (unless (fake-string? to)
    (error "String required"))
  (unless (fake-string? from)
    (error "String required"))
  (string-copy! (object-datum to) (%fixnum at)
                (object-datum from) (%fixnum start) (%fixnum end)))

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
(define (fake-bytevector-fill! obj b start end)
  ;; It is not in R7RS.
  (unless (fake-bytevector? obj)
    (error "Bytevector required"))
  (let ((bv (object-datum obj))
        (starti (%fixnum start))
        (endi (%fixnum end))
        (bi (%fixnum b)))
    (let loop ((idx starti))
     (unless (= idx endi)
       (bytevector-u8-set! bv idx bi)
       (loop (+ idx 1))))))
(define (fake-bytevector-copy! to at from start end)
  (unless (fake-bytevector? to)
    (error "Bytevector required"))
  (unless (fake-bytevector? from)
    (error "Bytevector required"))
  (bytevector-copy! (object-datum to) (%fixnum at)
                    (object-datum from) (%fixnum start) (%fixnum end)))

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
(define (fake-vector-fill! obj fill start end)
  (unless (fake-vector? obj)
    (error "Vector required"))
  (vector-fill! (object-datum obj) fill (%fixnum start) (%fixnum end)))
(define (fake-vector-copy! to at from start end)
  (unless (fake-vector? to)
    (error "Vector required"))
  (unless (fake-vector? from)
    (error "Vector required"))
  (vector-copy! (object-datum to) (%fixnum at)
                (object-datum from) (%fixnum start) (%fixnum end)))

;; yuni extension

;; flonum
;; FIXME: Currently, we lack "flonum cell" concept. 
;;        There is some room to improve coreops I/F for it
(define (fake-flonum? obj)
  (and (object? obj)
       (eq? tag-flonum (object-tag obj))))
(define (fake-wrap-flonum obj)
  (unless (and (number? obj) (inexact? obj))
    (error "Inexact number required" obj))
  (wrap-object tag-flonum obj))
(define (fake-unwrap-flonum fl)
  (unless (fake-flonum? fl)
    (error "Flonum required" fl))
  (object-datum fl))

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
(define (fake-make-simple-struct0 name count)
  ;; This one does not support initialization
  (wrap-object tag-simple-struct (cons name
                                       (make-vector (%fixnum count)))))
(define (fake-simple-struct-ref obj idx)
  (unless (fake-simple-struct? obj)
    (error "Simple-struct required" obj))
  (vector-ref (cdr (object-datum obj)) (%fixnum idx)))
(define (fake-simple-struct-set! obj idx x)
  (unless (fake-simple-struct? obj)
    (error "Simple-struct required"))
  (vector-set! (cdr (object-datum obj)) (%fixnum idx) x))
(define (fake-simple-struct-name obj)
  (unless (fake-simple-struct? obj)
    (error "Simple-struct required"))
  (car (object-datum obj)))

;; Primitive (callable)
(define (fake-primitive? obj)
  (and (object? obj)
       (eq? tag-primitive (object-tag obj))))
(define (fake-primitive-id obj)
  (object-datum obj))
(define (fake-make-primitive id)
  (wrap-object/zone1 tag-primitive (%fixnum id)))

;; VM closure (callable)
(define (fake-vmclosure? obj)
  (and (object? obj)
       (eq? tag-vmclosure (object-tag obj))))
(define (fake-vmclosure-env obj)
  (cdr (object-datum obj)))
(define (fake-vmclosure-label obj)
  (car (object-datum obj)))
(define (fake-make-vmclosure label env)
  (wrap-object tag-vmclosure (cons label env)))

;; Corelib
(define (fake-eq? a b)
  (or
    ;; Short-cut: If eq? on host, eq? on target
    (eq? a b)
    ;; null, boolean, char, symbols needed to be compared more precisely
    ;; ... boolean and null are not required do so though
    ;; (they should be host-eq? if they are target-eq?)
    ;; R7RS says nothing about eof-object
    (and (fake-symbol? a) (fake-symbol? b) (fake-symbol=?/2 a b))
    (and (fake-null? a) (fake-null? b))
    (and (fake-boolean? a) (fake-boolean? b) (fake-boolean=?/2 a b))
    (and (fake-char? a) (fake-char? b) (fake-char=?/2 a b))))

(define (fake-eqv? a b)
  (or
    ;; Short-cut: if eq? then eqv?
    (eq? a b)
    ;; Short-cut: if fake-eq? then eqv?
    (fake-eq? a b)
    ;; Numbers
    (and (fake-flonum? a) (fake-flonum? b)
         (let ((aa (fake-unwrap-flonum a))
               (bb (fake-unwrap-flonum b)))
           (= aa bb)))
    ;; we use host numbers and procedures
    (eqv? a b)))
         
(define (predicate1 proc)
  (lambda (obj) 
    (if (proc obj)
      (fake-true)
      (fake-false))))

(define (predicate2 proc)
  (lambda (a b) 
    (if (proc a b)
      (fake-true)
      (fake-false))))

;; For every predicate procedures, we have 2 variants
;;  1. returns host boolean
;;  2. returns target boolean
(define Pfake-eq?               (predicate2 fake-eq?))
(define Pfake-eqv?              (predicate2 fake-eqv?))
(define Pfake-null?             (predicate1 fake-null?))
(define Pfake-eof-object?       (predicate1 fake-eof-object?))
(define Pfake-boolean?          (predicate1 fake-boolean?))
(define Pfake-boolean=?/2       (predicate2 fake-boolean=?/2))
(define Pfake-true?             (predicate1 fake-true?))
(define Pfake-false?            (predicate1 fake-false?))
(define Pfake-char?             (predicate1 fake-char?))
(define Pfake-char=?/2          (predicate2 fake-char=?/2))
(define Pfake-string?           (predicate1 fake-string?))
(define Pfake-bytevector?       (predicate1 fake-bytevector?))
(define Pfake-symbol?           (predicate1 fake-symbol?))
(define Pfake-symbol=?/2        (predicate2 fake-symbol=?/2))
(define Pfake-pair?             (predicate1 fake-pair?))
(define Pfake-vector?           (predicate1 fake-vector?))
(define Pfake-simple-struct?    (predicate1 fake-simple-struct?))
(define Pfake-flonum?           (predicate1 fake-flonum?))
(define Pfake-fixnum?           (predicate1 fake-fixnum?))
(define Pfake-primitive?        (predicate1 fake-primitive?))
(define Pfake-vmclosure?        (predicate1 fake-vmclosure?))

;; VM registers
(define (fake-frame-set! f loc v) (vector-set! f loc v))
(define (fake-frame-ref f loc) (vector-ref f loc))
(define (fake-make-frame count) (make-vector count))
(define (fake-frame-length f) (vector-length f))
(define (fake-frame->list f) (vector->list f))
(define (fake-list->frame l) (list->vector l))
(define (fake-chain-last) '())
(define (fake-chain-last? x) (null? x))
(define (fake-chain-next x) (cdr x))
(define (fake-chain-current x) (car x))
(define (fake-chain-cons a b) (cons a b))
(define (fake-chain-ref c n) (list-ref c n))

;; No-op for host objects
(define (fake-heap-host-key k) k)
(define (fake-heap-host-fetch k) k)
         
(define (make-coreops-fake)

  (define (query sym)
    (case sym
      ((eq?)                 Pfake-eq?)
      ((Peq?)                fake-eq?)
      ((eqv?)                Pfake-eqv?)
      ((Peqv?)               fake-eqv?)
      ((null)                fake-null)
      ((null?)               Pfake-null?)
      ((Pnull?)              fake-null?)

      ((eof-object)          fake-eof-object)
      ((eof-object?)         Pfake-eof-object?)
      ((Peof-object?)        fake-eof-object?)

      ((true)                fake-true)
      ((false)               fake-false)
      ((boolean?)            Pfake-boolean?)
      ((Pboolean?)           fake-boolean?)
      (($boolean=?)          Pfake-boolean=?/2)
      ((P$boolean=?)         fake-boolean=?/2)
      ((true?)               Pfake-true?)
      ((false?)              Pfake-false?)
      ((Ptrue?)              fake-true?)
      ((Pfalse?)             fake-false?)

      ((char?)               Pfake-char?)
      ((Pchar?)              fake-char?)
      (($char=?)             Pfake-char=?/2)
      ((P$char=?)            fake-char=?/2)
      ((integer->char)       fake-integer->char)
      ((char->integer)       fake-char->integer)

      ((string?)             Pfake-string?)
      ((Pstring?)            fake-string?)
      ((string-length)       fake-string-length)
      ((string-ref)          fake-string-ref)
      ((string-set!)         fake-string-set!)
      (($make-string)        fake-make-string0)
      (($string-fill!)       fake-string-fill!) ;; extra
      (($string-copy!)       fake-string-copy!) ;; extra

      ((bytevector?)         Pfake-bytevector?)
      ((Pbytevector?)        fake-bytevector?)
      ((bytevector-length)   fake-bytevector-length)
      ((bytevector-u8-ref)   fake-bytevector-u8-ref)
      ((bytevector-u8-set!)  fake-bytevector-u8-set!)
      (($make-bytevector)    fake-make-bytevector0)
      (($bytevector-fill!)   fake-bytevector-fill!) ;; extra
      (($bytevector-copy!)   fake-bytevector-copy!) ;; extra

      ((symbol?)             Pfake-symbol?)
      (($symbol=?)           Pfake-symbol=?/2)
      ((Psymbol?)            fake-symbol?)
      ((P$symbol=?)          fake-symbol=?/2)
      ((string->symbol)      fake-string->symbol)
      ((symbol->string)      fake-symbol->string)

      ((pair?)               Pfake-pair?)
      ((Ppair?)              fake-pair?)
      ((cons)                fake-cons)
      ((car)                 fake-car)
      ((cdr)                 fake-cdr)
      ((set-car!)            fake-set-car!)
      ((set-cdr!)            fake-set-cdr!)

      ((vector?)             Pfake-vector?)
      ((Pvector?)            fake-vector?)
      ((vector-length)       fake-vector-length)
      ((vector-ref)          fake-vector-ref)
      ((vector-set!)         fake-vector-set!)
      (($make-vector)        fake-make-vector0)
      (($vector-fill!)       fake-vector-fill!) ;; extra
      (($vector-copy!)       fake-vector-copy!) ;; extra

      ((Pfixnum?)            fake-fixnum?)
      ((fixnum?)             Pfake-fixnum?)
      ((Pflonum?)            fake-flonum?)
      ((flonum?)             Pfake-flonum?)
      ((wrap-flonum)         fake-wrap-flonum)
      ((unwrap-flonum)       fake-unwrap-flonum)

      ((undefined)           fake-undefined)
      ((unspecified)         fake-unspecified)

      ((simple-struct?)      Pfake-simple-struct?)
      ((Psimple-struct?)     fake-simple-struct?)
      (($make-simple-struct) fake-make-simple-struct0)
      ((simple-struct-ref)   fake-simple-struct-ref)
      ((simple-struct-set!)  fake-simple-struct-set!)
      ((simple-struct-name)  fake-simple-struct-name)

      ((primitive?)          Pfake-primitive?)
      ((Pprimitive?)         fake-primitive?)
      ((make-primitive)      fake-make-primitive)
      ((primitive-id)        fake-primitive-id)

      ((vmclosure?)          Pfake-vmclosure?)
      ((Pvmclosure?)         fake-vmclosure?)
      ((make-vmclosure)      fake-make-vmclosure)
      ((vmclosure-env)       fake-vmclosure-env)
      ((vmclosure-label)     fake-vmclosure-label)

      ;; VM register implementations are here for performance
      ((HEAP-FRAME-SET!)     fake-frame-set!)
      ((HEAP-FRAME-REF)      fake-frame-ref)
      ((HEAP-MAKE-FRAME)     fake-make-frame)
      ((HEAP-FRAME-LENGTH)   fake-frame-length)
      ((HEAP-FRAME->LIST)    fake-frame->list)
      ((HEAP-LIST->FRAME)    fake-list->frame)
      ((HEAP-CHAIN-LAST)     fake-chain-last)
      ((HEAP-CHAIN-LAST?)    fake-chain-last?)
      ((HEAP-CHAIN-CURRENT)  fake-chain-current)
      ((HEAP-CHAIN-NEXT)     fake-chain-next)
      ((HEAP-CHAIN-CONS)     fake-chain-cons)
      ((HEAP-CHAIN-REF)      fake-chain-ref)
      ((HEAP-HOST-KEY)       fake-heap-host-key)
      ((HEAP-HOST-FETCH)     fake-heap-host-fetch)
      ((HEAP-SET-GC-HOOK!)   (lambda (cb) 'do-nothing))
      ((HEAP-GC-MARK!)       (lambda _ (error "Huh?")))

      (else (error "Unknown symbol" sym))))

  query)
)

