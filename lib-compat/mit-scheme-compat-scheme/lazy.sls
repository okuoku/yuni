(library (mit-scheme-compat-scheme lazy)
         (export
           delay delay-force
           force make-promise
           promise?)
         (import)

(define-primitive-names/yunifake
  force make-promise promise?)

(define-syntax-names/yunifake
  delay delay-force)
         
)
