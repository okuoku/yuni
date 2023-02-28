(library (yunife core-transformers)
         (export
           ;; Macro
           define-syntax/macro
           ;let-syntax/macro
           )
         (import (yuni scheme)
                 (yunife runtime synrules)
                 (yuniexternal chibi-scheme synrules))

;;

(define (define-syntax/macro name synrule)
  (let ((tran (yuni/syntax-rules-transformer
                synrule
                yuni/gensym (yuni/make-synrule-baselib) yuni/synrule-compare))
        (args (yuni/gensym 'args))
        (a (yuni/gensym 'output)))
    `(define-macro (,name . ,args)
       (let ((,a (,tran (cons (quote ,name) ,args)
                        yuni/gensym 
                        (yuni/make-synrule-baselib) yuni/synrule-compare)))
         ;(display (list 'OUT: ,a))
         ;(newline)
         ,a))))
         
)
