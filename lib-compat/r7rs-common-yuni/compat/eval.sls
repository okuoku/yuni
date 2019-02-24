(library (r7rs-common-yuni compat eval)
         (export eval/yuni eval environment)
         (import (scheme eval)
                 (scheme base))
         
(define (eval/yuni frm)
  (eval frm (environment '(yuni scheme))))

)
