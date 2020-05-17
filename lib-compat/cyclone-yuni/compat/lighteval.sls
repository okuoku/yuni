(library (cyclone-yuni compat lighteval)
         (export eval/yuni)
         (import (scheme eval)
                 (only (scheme base) define quote))

(define (eval/yuni frm)
  (eval frm))
)
