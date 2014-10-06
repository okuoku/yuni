(library (r7b-util define-values)
         (export define-values)
         (import (rnrs) (rnrs mutable-pairs))

         
;; Took from 7.3 Derived expression types

(define-syntax define-values
  (syntax-rules ()
    ((define-values () expr)
     (define dummy
       (call-with-values (lambda () expr)
                         (lambda args '#f))))
    ((define-values (var) expr)
     (define var expr))
    ((define-values (var0 var1 ... varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
                           list))
       (define var1
         (let ((v (cadr var0)))
          (set-cdr! var0 (cddr var0))
          v)) ...
       (define varn
         (let ((v (cadr var0)))
          (set! var0 (car var0))
          v))))
    ((define-values (var0 var1 ... . varn) expr)
     (begin
       (define var0
         (call-with-values (lambda () expr)
                           list))
       (define var1
         (let ((v (cadr var0)))
          (set-cdr! var0 (cddr var0))
          v)) ...
       (define varn
         (let ((v (cdr var0)))
          (set! var0 (car var0))
          v))))
    ((define-values var expr)
     (define var
       (call-with-values (lambda () expr)
                         list)))))
)
