(library (r7c-report binding-construct let-values)
         (export let-values let*-values)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax letcore)
                 (r7c syntax lambda)
                 (r7c core values))

;; Took from 7.3 Derived expression types
;;    let => $let/core
;;    Use __1 to denote gensym
(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body0 body1 ...)
     (let-values "bind"
         (binding ...) () (begin body0 body1 ...)))
    ((let-values "bind" () tmps body)
     ($let/core tmps body))
    ((let-values "bind" ((b0 e0)
         binding ...) tmps body)
     (let-values "mktmp" b0 e0 ()
         (binding ...) tmps body))
    ((let-values "mktmp" () e0 args
         bindings tmps body)
     (call-with-values
       (lambda () e0)
       (lambda args
         (let-values "bind"
             bindings tmps body))))
    ((let-values "mktmp" (a . b) e0 (arg ...)
         bindings (tmp ...) body)
     (let-values "mktmp" b e0 (arg ... __1)
         bindings (tmp ... (a __1)) body))
    ((let-values "mktmp" a e0 (arg ...)
        bindings (tmp ...) body)
     (call-with-values
       (lambda () e0)
       (lambda (arg ... . __1)
         (let-values "bind"
             bindings (tmp ... (a __1)) body))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body0 body1 ...) 
     ($let/core () body0 body1 ...))
    ((let*-values (binding0 binding1 ...)
         body0 body1 ...)
     (let-values (binding0)
       (let*-values (binding1 ...)
         body0 body1 ...)))))
)
