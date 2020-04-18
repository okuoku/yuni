(import (yuni scheme)
        (yunitest mini))

;; R7RS
(check-equal '#(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)) ;; S7

(check-finish)
