(library (r6rs-common-yuni compat eval)
         (export eval/yuni eval environment)
         (import (rnrs eval)
                 (only (rnrs)
                       define quote))

(define (eval/yuni frm)
  (eval frm (environment '(yuni scheme))))

)
