(library (yunivmrt keywords)
         (export define-keywords
                 define-syntax-rules/keywords)
         (import (yunifake scheme))

(define-syntax define-syntax-rules/keywords
  (syntax-rules ()
    ((_ nam (symlit ...) (keylit ...) clauses ...)
     (define-syntax nam
       (syntax-rules (symlit ... keylit ...)
         clauses ...)))))

(define-syntax define-keywords
  (syntax-rules ()
    ((_ key ...)
     (begin 'do-nothing-define-keywords))))
         
)
