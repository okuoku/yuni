(library (r6rs-common-yuni compat lighteval)
         (export eval/yuni)
         (import (rnrs eval)
                 (only (rnrs)
                       define quote))

(define (eval/yuni frm)
  (eval frm (environment '(yuni scheme))))

)
