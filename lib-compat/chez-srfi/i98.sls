(library (chez-srfi i98)
         (export get-environment-variable
                 get-environment-variables)
         (import (rename (chezscheme) (getenv get-environment-variable)))
         
(define (get-environment-variables)
  '(("QQQ_ERROR" . "FIXME: Chez scheme do not have get-environment-variables")))         
         
)
