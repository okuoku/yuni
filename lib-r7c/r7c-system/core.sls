(library (r7c-system core)
         (export define-syntax
                 syntax-rules
                 syntax-error
                 quote
                 begin set!)
         (import (r7c-expander-interface))

;; FIXME: We do not have quote _ or ... here. 
         
(define-syntax begin
  (syntax-rules ()
    ((begin? . rest) ($inject/splice begin . rest))))         

(define-syntax set!
  (syntax-rules ()
    ((set!? var arg) ($inject set! var arg))))

(define-syntax quote
  (syntax-rules ()
    ((quote? obj) ($inject quote ($quote obj)))))
         
)
