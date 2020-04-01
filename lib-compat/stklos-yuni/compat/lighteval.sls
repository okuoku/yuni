(library (stklos-yuni compat lighteval)
         (export eval/yuni)
         (import)

(define (eval/yuni sexp) (eval sexp (interaction-environment)))
)
