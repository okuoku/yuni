(library (chicken-yuni compat lighteval)
         (export eval/yuni)
         (import (yuni scheme)
                 (chicken eval))

(define (eval/yuni code)
  ((eval-handler) code))         
         
)
