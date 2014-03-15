(library (gauche-yuni compat keywords)
         (export define-keywords
                 syntax-rules/keywords)
         (import (yuni scheme))

(define-syntax syntax-rules/keywords
  (syntax-rules ()
    ((_ symlit* keylit* clauses ...)
     (syntax-rules symlit*
       clauses ...))))

(define-syntax define-keywords
  (syntax-rules ()
    ((_ bogus ...) (begin))))
         
)
