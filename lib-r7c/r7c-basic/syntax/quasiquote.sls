(library (r7c-basic syntax quasiquote)
         (export quasiquote unquote unquote-splicing)
         (import (r7c-system core)
                 (r7c heap pair)
                 (r7c heap list)
                 (r7c heap listloop)
                 (r7c heap vector)
                 (r7c-system auxsyntax)
                 (r7c-system synrules))
         
;; Basic idea took from EIOD.scm (by Al Petrofsky)
(define-syntax $$yunifake-qq
  (syntax-rules ($$yunifake-qq quasiquote unquote unquote-splicing)
    ;; Depth == 0
    ((_ (unquote input)) input)
    ((_ ((unquote-splicing x) . y))
     ;; Reenter
     ($append x ($$yunifake-qq y)))

    ;; Outputmyself1
    ((_ (quasiquote x) . depth)
     ;; Depth++
     (cons 'quasiquote ($$yunifake-qq (x) depth)))
    ;; Outputmyself2
    ((_ ($$yunifake-qq x) . depth)
     ;; Depth++
     (cons 'quasiquote ($$yunifake-qq (x) depth)))

    ;; Outputunquote
    ((_ (unquote x) depth)
     ;; Depth--
     (cons 'unquote ($yunifake-qq (x) . depth)))
    ;; Outputunquote-splicing
    ((_ (unquote-splicing x) depth)
     (cons 'unquote-splicing ($$yunifake-qq (x) . depth)))

    ;; Outputcons
    ((_ (a . d) . depth)
     ;; Depth no change
     (cons ($$yunifake-qq a . depth)
           ($$yunifake-qq d . depth)))

    ;; Outputvector
    ((_ #(lis ...) . depth)
     ;; Depth no change
     (list->vector ($$yunifake-qq (lis ...) . depth)))

    ;; Terminate
    ((_ input . any) 'input)))

(define-syntax quasiquote ;; Depth zero, entrypoint
  (syntax-rules ()
    ;; Reject (quasiquote 1 2 ...) case
    ((_ input)
     ($$yunifake-qq input))))
         
)
