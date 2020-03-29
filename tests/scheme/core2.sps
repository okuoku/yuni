(import (yuni scheme)
        (yunitest mini))

;; lists

;; list-copy (non-pair)
(check-equal 'a (list-copy 'a))
(check-equal '() (list-copy '()))
(check-equal '(10 . 20) (list-copy '(10 . 20)))

(check-finish)
