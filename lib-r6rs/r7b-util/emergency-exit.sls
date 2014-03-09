(library (r7b-util emergency-exit)
         (export emergency-exit)
         (import (rnrs))
;; FIXME: it should map to _exit
(define (emergency-exit . obj)
  (apply exit obj))
)
