(library (r7c-basic syntax definecore)
         (export $let/core)
         (import (r7c-system expander)
                 (r7c-system synrules)
                 (r7c-system core))

(define-syntax $define/core
  (syntax-rules ()
    ((_ nam obj)
     ($inject define
              ($bind-definition nam)
              ($extend-env (nam) obj)))))
)
