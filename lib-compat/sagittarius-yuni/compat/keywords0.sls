(library (sagittarius-yuni compat keywords0)
         (export define-keywords
                 define-syntax-rules/keywords)
         (import (yuni scheme))
         
;; Same as (gauche-yuni compat keywords)
(define-syntax define-syntax-rules/keywords
  (syntax-rules ()
    ((_ nam symlit* keylit* clauses ...)
     (define-syntax nam 
       (syntax-rules symlit*
         clauses ...)))))

(define-syntax define-keywords
  (syntax-rules ()
    ((_ bogus ...) (begin))))

)
