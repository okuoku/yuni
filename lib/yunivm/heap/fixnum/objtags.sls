(library (yunivm heap fixnum objtags)
         (export 
           fixnum-null
           fixnum-null?
           fixnum-eof-object
           fixnum-eof-object?
           fixnum-true
           fixnum-false
           fixnum-true?
           fixnum-false?
           fixnum-boolean?
           fixnum-boolean=?
           fixnum-char?
           fixnum-char=?
           fixnum-integer->char
           fixnum-char->integer
           fixnum-fixnum?
           fixnum-string?
           fixnum-idx->string
           fixnum-string->idx
           fixnum-bytevector?
           fixnum-idx->bytevector
           fixnum-bytevector->idx
           fixnum-symbol?
           fixnum-symbol=?
           fixnum-symbol->idx
           fixnum-idx->symbol
           fixnum-pair?
           fixnum-idx->pair
           fixnum-pair->idx
           fixnum-vector?
           fixnum-idx->vector
           fixnum-vector->idx
           fixnum-flonum?
           fixnum-idx->flonum
           fixnum-flonum->idx
           fixnum-undefined
           fixnum-unspecified
           fixnum-simple-struct?
           fixnum-idx->simple-struct
           fixnum-simple-struct->idx
           fixnum-primitive?
           fixnum-primitive-id
           fixnum-make-primitive
           fixnum-vmclosure?
           fixnum-idx->vmclosure
           fixnum-vmclosure->idx)
         (import (yuni scheme))

;;
         
(define (%fixnum-offset-require val offs check)
  (let ((out (+ val offs)))
   (unless (check out)
     (error "Overflow" check val))
   out))

;; null
(define (fixnum-null) #x70000000)
(define (fixnum-null? obj) (= obj #x70000000))
;; eof-object
(define (fixnum-eof-object) #x70000001)
(define (fixnum-eof-object? obj) (= obj #x70000001))
;; boolean -- true / false
(define (fixnum-true) #x70000004)
(define (fixnum-false) #x70000005)
(define (fixnum-true? x) (= x #x70000004))
(define (fixnum-false? x) (= x #x70000005))
(define (fixnum-boolean? obj) (or (fixnum-true? obj)
                                  (fixnum-false? obj)))
(define (fixnum-boolean=? a b) (= a b))
;; char
(define (fixnum-char? obj) (<= #x14000000 obj #x17FFFFFF))
(define (fixnum-char=? a b) (= a b))
(define (fixnum-integer->char int)
  (%fixnum-offset-require int #x14000000 fixnum-char?))
(define (fixnum-char->integer obj)
  (unless (fixnum-char? obj)
    (error "Char required" obj))
  (- obj #x14000000))

;; fixnum
(define (fixnum-fixnum? obj) (<= -268435456 obj #xFFFFFFF))

;; string
(define (fixnum-string? obj) (<= -402653184 obj -268435457))
(define (fixnum-idx->string idx)
  (%fixnum-offset-require idx -402653184 fixnum-string?))
(define (fixnum-string->idx obj)
  (unless (fixnum-string? obj)
    (error "String required" obj))
  (- obj -402653184))

;; bytevector
(define (fixnum-bytevector? obj) (<= -805306368 obj -671088641))
(define (fixnum-idx->bytevector idx)
  (%fixnum-offset-require idx -805306368 fixnum-bytevector?))
(define (fixnum-bytevector->idx obj)
  (unless (fixnum-bytevector? obj)
    (error "Bytevector required" obj))
  (- obj -805306368))

;; symbol
(define (fixnum-symbol? obj) (<= #x1C000000 obj #x1FFFFFFF))
(define (fixnum-symbol=? a b) (= a b))
(define (fixnum-symbol->idx obj)
  (unless (fixnum-symbol? obj)
    (error "Symbol required" obj))
  (- obj #x1C000000))
(define (fixnum-idx->symbol idx)
  (%fixnum-offset-require idx #x1C000000 fixnum-symbol?))

;; pair
(define (fixnum-pair? obj) (<= -2147483648 obj -2013265921))
(define (fixnum-idx->pair idx)
  (%fixnum-offset-require idx -2147483648 fixnum-pair?))
(define (fixnum-pair->idx obj)
  (unless (fixnum-pair? obj)
    (error "Pair required" obj))
  (- obj -2147483648))

;; vector
(define (fixnum-vector? obj) (<= -1073741824 obj -939524097))
(define (fixnum-idx->vector idx)
  (%fixnum-offset-require idx -1073741824 fixnum-vector?))
(define (fixnum-vector->idx obj)
  (unless (fixnum-vector? obj)
    (error "Vector required" obj))
  (- obj -1073741824))

;; flonum
(define (fixnum-flonum? obj) (<= -671088640 obj -536870913))
(define (fixnum-idx->flonum idx)
  (%fixnum-offset-require idx -671088640 fixnum-flonum?))
(define (fixnum-flonum->idx obj)
  (unless (fixnum-flonum? obj)
    (error "Flonum required" obj))
  (- obj -671088640))

;; undefined
(define (fixnum-undefined) #x70000003)

;; unspecified
(define (fixnum-unspecified) #x70000002)

;; simple-struct
(define (fixnum-simple-struct? obj) (<= -939524096 obj -805306369))
(define (fixnum-idx->simple-struct idx)
  (%fixnum-offset-require idx -939524096 fixnum-simple-struct?))
(define (fixnum-simple-struct->idx obj)
  (unless (fixnum-simple-struct? obj)
    (error "Simple-struct required" obj))
  (- obj -939524096))

;; primitive
(define (fixnum-primitive? obj) (<= #x18000000 obj #x1BFFFFFF))
(define (fixnum-primitive-id obj)
  (unless (fixnum-primitive? obj)
    (error "Primitive required" obj))
  (let ((offs (- obj #x18000000)))
   (if (> offs #x1FFFFFF)
     (- #x3fffffe offs)
     offs)))
(define (fixnum-make-primitive id)
  (let ((offs (if (< id 0) (+ #x3ffffff id) id)))
   (%fixnum-offset-require offs #x18000000 fixnum-primitive?)))

;; VM closure
(define (fixnum-vmclosure? obj) (<= -1476395008 obj -1342177281))
(define (fixnum-idx->vmclosure idx)
  (%fixnum-offset-require idx -1476395008 fixnum-vmclosure?))
(define (fixnum-vmclosure->idx obj)
  (unless (fixnum-vmclosure? obj)
    (error "Vmclosure required" obj))
  (- obj -1476395008))

)
