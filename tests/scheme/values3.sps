;; non-values object
(import (yuni scheme)
        (yunitest mini))

(call-with-values
  (lambda () 1)
  (lambda a (check-equal a '(1))))

(check-finish)
