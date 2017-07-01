(library (gambit-compat-scheme inexact)
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
  exp finite?
  infinite?
  nan? sin
  sqrt tan)
         
)
