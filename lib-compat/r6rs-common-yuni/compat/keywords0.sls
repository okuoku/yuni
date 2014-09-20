(library (r6rs-common-yuni compat keywords0)
         (export define-keywords
                 syntax-rules/keywords)
         (import (yuni scheme)
                 (yuni util invalid-form))

         
(define-syntax syntax-rules/keywords
  (syntax-rules ()
    ((_ (symlit ...) (keylit ...) clauses ...)
     (syntax-rules (symlit ... keylit ...)
       clauses ...)))) 

(define-syntax define-keywords
  (syntax-rules ()
    ((_ key ...)
     (define-invalid-forms key ...))))

)
