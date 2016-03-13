(library (r7c-basic syntax quasiquote)
         (export quasiquote unquote unquote-splicing)
         (import (r7c-system core)
                 (r7c heap pair)
                 (r7c heap list)
                 (r7c heap vector)
                 (r7c-system auxsyntax)
                 (r7c-system synrules))

         
;; Basic idea took from EIOD.scm (by Al Petrofsky)
(define-syntax qq
  (syntax-rules (quasiquote unquote unquote-splicing)
    ;; Depth 0
    ((_ (unquote x)) x)
    ((_ ((unquote-splicing x) . y)) (append x (qq y)))
    ((_ (quasiquote x) . d)     (cons 'quasiquote (qq (x) d)))
    ;; Depth 1+
    ((_ (unquote x) d)          (cons 'unquote (qq (x) . d)))
    ((_ (unquote-splicing x) d) (cons 'unquote-splicing (qq (x) . d)))
    ;; Iter
    ((_ (x . y) . d)    (cons (qq x . d) (qq y . d)))
    ((_ #(x ...) . d) (list->vector (qq (x ...) . d)))
    ;; Term
    ((_ x . d) 'x))) 

(define-syntax quasiquote
  (syntax-rules ()
    ((_ x)
     (qq x))))
         
)
