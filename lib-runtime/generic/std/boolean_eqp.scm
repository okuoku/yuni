(define (boolean=? first next . rest)
  (unless (boolean? first)
    (error "Boolean required" first))
  (unless (boolean? next)
    (error "Boolean required" next))
  (and (eqv? first next)
       (or (null? rest)
           (apply boolean=? next rest))))

