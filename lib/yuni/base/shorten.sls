(library (yuni base shorten)
         (export 
           ^
           ^a ^b ^c ^d ^e ^f ^g ^h ^i ^j ^k ^l ^m
           ^n ^o ^p ^q ^r ^s ^t ^u ^v ^w ^x ^y ^z ^_

;#|
;           ^a* ^b* ^c* ^d* ^e* ^f* ^g* ^h* ^i* ^j* ^k* ^l* ^m*
;           ^n* ^o* ^p* ^q* ^r* ^s* ^t* ^u* ^v* ^w* ^x* ^y* ^z* ^_*
;|#
           
           )
         (import (scheme base)
                 (yuni compat macro primitives))
         (begin
;;

(define-syntax ^
  (syntax-rules ()
    ((_ rest ...)
     (lambda rest ...))))

(define-syntax define-^*
  (syntax-rules ()
    ((_ (name sym) ...)
     (begin
       (define-syntax ^body
         (syntax-rules ()
           ((_ k varname . body)
            (k (lambda (varname) . body)))))
       (define-inject-syntax 
         name
         (sym)
         ^body)
       ...))))

(define-^*
  (^a a) (^b b) (^c c) (^d d) (^e e) (^f f) (^g g)
  (^h h) (^i i) (^j j) (^k k) (^l l) (^m m) (^n n)
  (^o o) (^p p) (^q q) (^r r) (^s s) (^t t) (^u u)
  (^v v) (^w w) (^x x) (^y y) (^z z)
  (^_ _))

))
