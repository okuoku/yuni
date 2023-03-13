(import (yuni scheme)
        (yunitest mini))

(check-equal 1 (* 1 1))
(check-equal 4 (- 5 1))

(check-equal #t (= 0 0))
(check-equal #t (= 123 123))
(check-equal #t (< 1 2))
(check-equal #f (> 1 2))
(check-equal #f (>= 1 2))
(check-equal #f (> 3 3))
(check-equal #f (< 3 3))
(check-equal #t (>= 3 3))
(check-equal #t (<= 3 3))

(check-equal 1 (+ 1))
(check-equal 3 (+ 1 1 1))
(check-equal 0 (+))
(check-equal 1 (*))
(check-equal 0 (* 1 123 0))

(check-finish)
