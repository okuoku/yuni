(library (s7-yuni compat keywords)
         (export define-keywords
                 define-syntax-rules/keywords)
         (import (yuni scheme)
                 (yuni util invalid-form)
                 )

;;

(define-syntax define-syntax-rules/keywords
  (syntax-rules ()
    ((_ nam (symlit ...) (keylit ...) clauses ...)
     (define-syntax nam
       (syntax-rules (symlit ... keylit ...)
         clauses ...)))))

(define-syntax define-keywords
  (syntax-rules ()
    ((_ key ...)
     ;; Ignore keywords
     (begin))))
         
)
