(library (r7c-report conditional unless)
         (export unless)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax if)
                 (r7c heap boolean))


;; Took from 7.3 Derived expression types
(define-syntax unless
  (syntax-rules ()
    ((unless test result1 result2 ...)
     (if (not test)
       (begin result1 result2 ...)))))
)
