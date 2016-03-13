(library (r7c-report conditional and)
         (export and)
         (import 
           (r7c syntax if)
           (r7c-system synrules)
           (r7c-system core))


;; Took from 7.3 Derived expression types
(define-syntax and
  (syntax-rules ()
    ((and) '#t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) '#f))))
)
