(library (r7c-basic syntax letcore)
         (export $let/core)
         (import (r7c-system expander)
                 (r7c-system synrules)
                 (r7c-system core))

(define-syntax $let/core
  (syntax-rules ()
    ((_ ((nam frm) ...) body ...)
     ($inject let
              ((($bind-variable nam) frm) ...)
              ($extend-env (nam ...)
                           body ...)))))

)
