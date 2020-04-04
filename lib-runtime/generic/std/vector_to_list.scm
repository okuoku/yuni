(define (vector->list/itr acc v cur start end)
  (if (= end cur)
    (reverse acc)
    (vector->list/itr (cons (vector-ref v cur) acc) v (+ 1 cur) start end)))

(define vector->list
  (case-lambda
    ((v) (vector->list v 0 (vector-length v)))
    ((v start) (vector->list v start (vector-length v)))
    ((v start end) 
     (let ((len (- end start)))
       (vector->list/itr '() v 0 start end)))))
