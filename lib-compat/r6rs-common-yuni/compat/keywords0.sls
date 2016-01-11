(library (r6rs-common-yuni compat keywords0)
         (export define-keywords
                 define-syntax-rules/keywords)
         (import (for (yuni scheme) run (meta -1))
                 (yuni util invalid-form))

         
(define-syntax define-syntax-rules/keywords
  (syntax-rules ()
    ((_ nam (symlit ...) (keylit ...) clauses ...)
     (define-syntax nam
       (syntax-rules (symlit ... keylit ...)
         clauses ...))))) 

(define-syntax define-keywords
  (syntax-rules ()
    ((_ key ...)
     (define-invalid-forms key ...))))

)
