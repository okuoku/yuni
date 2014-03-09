(library (yuni binary walk flags)
         (export decompose-flags)
         (import (rnrs))


;; bit-flag decomposition utility

(define (decompose-flags flags num) ;; => (residue . flags)
  ;; flags = ((SYM . value) ...)
  (define cur num)
  (define flags '())
  (define (add p)
    (let ((code (cdr p))
          (sym (car p)))
      (unless (= 0 (bitwise-and code cur))
        (set! cur (- cur code))
        (set! flags (cons sym flags)))))
  (cons cur flags))

)
