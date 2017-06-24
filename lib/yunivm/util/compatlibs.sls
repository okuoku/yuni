(library (yunivm util compatlibs)
         (export 
           compatlibs-proc-vector
           compatlibs-name-vector)
         (import (yuni scheme)
                 (yuni compat simple-struct)) 

(define ($undefined) (if #f #f))
(define $append append)
(define $fx-length length)
(define $fx= =)
(define $fx<= <=)
(define $fx>= >=)
(define $fx< <)
(define $fx> >)
(define $fx+ +)
(define $fx- -)

(define $make-string make-string)
(define $make-bytevector make-bytevector)
(define ($make-vector len) (make-vector len #f))

(define $boolean=? boolean=?)
(define $char=? char=?)
(define $symbol=? symbol=?)

(define compatlibs-proc-vector
  (vector
    ;; Non-standard core ops 
    $undefined
    $append
    $fx-length
    $fx=
    $fx<=
    $fx>=
    $fx<
    $fx>
    $fx+
    $fx-
    $make-string
    $make-bytevector
    $make-vector
    $boolean=?
    $char=?
    $symbol=?
    ;; Simple-struct
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
    )
  )         

(define compatlibs-name-vector
  '#(
    ;; Non-standard core ops 
    $undefined
    $append
    $fx-length
    $fx=
    $fx<=
    $fx>=
    $fx<
    $fx>
    $fx+
    $fx-
    $make-string
    $make-bytevector
    $make-vector
    $boolean=?
    $char=?
    $symbol=?
    ;; Simple-struct
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
     ))
         
         
)
