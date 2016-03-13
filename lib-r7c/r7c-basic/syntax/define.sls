(library (r7c-basic syntax define)
         (export define)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax lambda)
                 (r7c syntax definecore))

(define-syntax define
  (syntax-rules ()
    ((_ (nam . frm) body ...)
     ($define/core nam (lambda frm body ...)))
    ((_ nam body)
     ($define/core nam body))))         
         
)
