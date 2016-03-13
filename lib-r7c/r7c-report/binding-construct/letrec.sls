(library (r7c-report binding-construct letrec)
         (export letrec letrec*)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax letcore)
                 (r7c heap undefined))


;; Took from 7.3 Derived expression types
;;  <undefined> => ($undefined)
;;  let => $let/core
(define-syntax letrec
  (syntax-rules ()
    ((letrec ((var1 init1) ...) body ...) 
     (letrec "generate temp names"
       (var1 ...)
       ()
       ((var1 init1) ...)
       body ...))
    ((letrec "generate temp names" ()
       (temp1 ...)
       ((var1 init1) ...)
       body ...)
     ($let/core ((var1 ($undefined)) ...)
       ($let/core ((temp1 init1) ...)
         (set! var1 temp1)
         ...
         body ...)))
    ((letrec "generate temp names" (x y ...)
       (temp ...)
       ((var1 init1) ...)
       body ...)
     (letrec "generate temp names" (y ...)
       (newtemp temp ...)
       ((var1 init1) ...)
       body ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     ($let/core ((var1 ($undefined)) ...)
               (set! var1 init1)
               ...
               ($let/core () body1 body2 ...)))))
)
