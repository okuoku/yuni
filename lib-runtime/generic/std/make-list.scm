(define (make-list/fill cur k fil)
  (if (>= 0 k)
    cur
    (make-list/fill (cons fil cur) (- k 1) fil)))

(define (make-list k . fill?)
  (if (null? fill?)
    (make-list/fill '() k #f)
    (make-list/fill '() k (car fill?))))

