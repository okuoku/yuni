(library (sagittarius-yuni compat keywords0)
         (export define-keywords
                 syntax-rules/keywords)
         (import (yuni scheme))
         
;; Same as (gauche-yuni compat keywords)
(define-syntax syntax-rules/keywords
  (syntax-rules ()
    ((_ symlit* keylit* clauses ...)
     (syntax-rules symlit*
       clauses ...))))

(define-syntax define-keywords
  (syntax-rules ()
    ((_ bogus ...) (begin))))

)
