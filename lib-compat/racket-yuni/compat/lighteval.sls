(library (racket-yuni compat lighteval)
         (export eval/yuni)
         (import 
           (rnrs)
           (only (racket base)
                 current-namespace
                 namespace-variable-value))

(define eval/yuni
  (namespace-variable-value 
    'eval/yuni
    #t
    #f
    (current-namespace))
  )

)
