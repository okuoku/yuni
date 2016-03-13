(library (r7c-report conditional when)
         (export when)
         (import 
           (r7c syntax if)
           (r7c-system synrules)
           (r7c-system core))


;; Took from 7.3 Derived expression types
(define-syntax when
  (syntax-rules ()
    ((when test result1 result2 ...)
     (if test
       (begin result1 result2 ...)))))
)
