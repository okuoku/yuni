(library (bigloo-yuni compat lighteval)
         (export eval/yuni)
         (import (yuni scheme))

(define (eval/yuni frm)
  (eval frm (interaction-environment)))
)
