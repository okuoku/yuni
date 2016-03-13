(library (r7c-report conditional or)
         (export or)
         (import 
           (r7c syntax if)
           (r7c syntax letcore)
           (r7c-system synrules)
           (r7c-system core))


;; Took from 7.3 Derived expression types
(define-syntax or
  (syntax-rules ()
    ((or) '#f)
    ((or test) test)
    ((or test1 test2 ...)
     ($let/core ((x test1))
       (if x x (or test2 ...))))))
)
