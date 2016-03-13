(library (r7c-basic syntax lambda)
         (export lambda)
         (import (r7c-system core)
                 (r7c-system expander)
                 (r7c-system synrules))

(define-syntax lambda
  (syntax-rules ()
    ((_ frm body ...)
     ($inject lambda
              ($extend-env frm body ...))))) 

)
