;; Exceptions + parameters
(import (yuni scheme)
        (yunitest mini))

; Chez does not compatible with R7RS for parameters with converter
; https://github.com/cisco/ChezScheme/issues/409

(define p0 (make-parameter 0))
(define p1 (make-parameter 0))

(check-equal 0 (p0))

#|
; Digamma, Chicken, s7 and STklos does not support this form
(check-equal 'abcd
             (parameterize () 'abcd))
|#

(check-equal 'done
             (parameterize
               ((p0 1234))
               (check-equal 1234 (p0))
               'done))

(check-equal 0 (p0))
(check-equal 0 (p1))

(check-equal 'done2
             (parameterize
               ((p0 1234)
                (p1 5678))
               (guard (cnd
                        (#t 
                         (check-equal 1234 (p0))
                         (check-equal 5678 (p1))
                         'done2))
                      (error "err")
                      'done3)))

(check-finish)
