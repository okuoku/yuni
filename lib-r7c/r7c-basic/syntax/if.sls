(library (r7c-basic syntax if)
         (export if)
         (import (r7c-system core)
                 (r7c-system expander)
                 (r7c-system synrules))

(define-syntax if
  (syntax-rules ()
    ((_ a b)
     ($inject if a b))
    ((_ a b c)
     ($inject if a b c))))

         
)
