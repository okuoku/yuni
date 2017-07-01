(library (mit-scheme-compat-scheme inexact)
         (export
           cos asin
           atan acos
           exp finite?
           infinite? log
           nan? sin
           sqrt tan)
         (import)

(define log
  (case-lambda ((a) ($log a))
               ((a b) (inexact (/ ($log a) ($log b))))))

(define-primitive-names/yunifake
  cos asin
  atan acos
  exp 
  infinite?
  sin
  sqrt tan)

(define nan? 'YUNIFAKE-UNIMPLEMENTED)
(define (finite? x) (or (and (number? x) (integer? x)) 
                        (flo:finite? x)))
         
)
