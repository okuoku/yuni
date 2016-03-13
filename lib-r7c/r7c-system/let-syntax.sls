(library (r7c-system let-syntax)
         (export let-syntax
                 letrec-syntax)
         (import (r7c-system core)
                 (r7c-expander-interface)
                 (r7c syntax letcore)
                 (r7c-system synrules))

(define-syntax letrec-syntax
  (syntax-rules ()
    ((_ ((nam trans) ...) body ...)
     ($let/core ()
                (define-syntax nam trans)
                ...
                ($let/core () body ...)))))

(define-syntax let-syntax
  (syntax-rules ()
    ((_ () body ...) ;; term
     ($let/core () body ...))
    ((_ ((nam trans) . rest) body ...)
     ;; Rename this step
     ($let/core ()
                (define-syntax temp trans)
                (let-syntax rest
                  ($alias nam temp)
                  body ...)))))

)
