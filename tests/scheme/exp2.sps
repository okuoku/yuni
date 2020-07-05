(import (yuni scheme)
        (yunitest mini))

; Guile and MIT/GNU Scheme does not follow dynamic-scoping rule

(define p0 (make-parameter 0))

(check-equal 'done4
             (parameterize
               ((p0 1))
               (check-equal 1 (p0))
               (guard
                 (cnd (#t
                       (check-equal 1 (p0))
                       'done4))
                 (check-equal 1 (p0))
                 (parameterize
                   ((p0 2))
                   (check-equal 2 (p0))
                   (raise 'bogus)))))

(check-finish)
