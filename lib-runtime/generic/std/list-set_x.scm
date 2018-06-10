(define (list-set! lis k v)
  (set-car! (list-tail lis k) v))

