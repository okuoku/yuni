(define (vector->string/itr! s v cur start end)
  (unless (= end cur)
    (string-set! s cur (vector-ref v cur))
    (vector->string/itr! s v (+ 1 cur) start end)))

(define vector->string
  (case-lambda
    ((v) (vector->string v 0 (vector-length v)))
    ((v start) (vector->string v start (vector-length v)))
    ((v start end) 
     (let* ((len (- end start))
            (s (make-string len)))
       (vector->string s v 0 start end)
       s))))
