(library (r7c-report conditional cond)
         (export cond else =>)
         (import 
           (r7c-system synrules)
           (r7c-system auxsyntax)
           (r7c syntax if)
           (r7c-system core))


;; Took from 7.3 Derived expression types
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
         (result temp)
         (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp temp
         (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
       (begin result1 result2 ...)
       (cond clause1 clause2 ...)))))

)
