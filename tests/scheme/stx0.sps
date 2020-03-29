(import (yuni scheme)
        (yunitest mini))

(define ax 0)

;; NMosh scoping issue: https://github.com/okuoku/yuni/issues/83
(begin
  (let ((ax 0))
   (set! ax 1)
   (check-equal ax 1))
  (check-equal ax 0))

(let ((ay 0))
 (let ((ay 0))
  (define ay 1)
  (set! ay 1)
  (check-equal ay 1))
 (check-equal ay 0))

(check-finish)
