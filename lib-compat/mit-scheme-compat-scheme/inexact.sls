(library (mit-scheme-compat-scheme inexact)
         (export
           cos asin
           atan acos
           exp finite?
           infinite? log
           nan? sin
           sqrt tan)
         (import)

(define-primitive-names/yunifake
  cos asin
  atan acos
  exp finite?
  infinite? log
  nan? sin
  sqrt tan)
         
)
