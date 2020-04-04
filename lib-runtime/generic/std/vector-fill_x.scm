(define (vector-fill!/itr v fill start end)
  (unless (= start end)
    (vector-set! v start fill)
    (vector-fill!/itr v fill (+ start 1) end)))
(define vector-fill!
  (case-lambda
    ((v fill)
     (vector-fill! v fill 0 (vector-length v)))
    ((v fill start)
     (vector-fill! v fill start (vector-length v)))
    ((v fill start end)
     (vector-fill!/itr v fill start end))))
