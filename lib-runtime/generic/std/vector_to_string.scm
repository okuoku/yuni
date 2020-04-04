(define (vector->string/itr! s v off cur end)
  (unless (= end cur)
    (string-set! s off (vector-ref v cur))
    (vector->string/itr! s v (+ 1 off) (+ 1 cur) end)))

(define vector->string
  (case-lambda
    ((v) (vector->string v 0 (vector-length v)))
    ((v start) (vector->string v start (vector-length v)))
    ((v start end) 
     (let* ((len (- end start))
            (s (make-string len)))
       (vector->string/itr! s v 0 start end)
       s))))
