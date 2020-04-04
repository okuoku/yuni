(define (string->vector/itr! v s off cur end)
  (unless (= end cur)
    (vector-set! v off (string-ref s cur))
    (string->vector/itr! v s (+ 1 off) (+ 1 cur) end)))

(define string->vector
  (case-lambda
    ((s) (string->vector s 0 (string-length s)))
    ((s start) (string->vector s start (string-length s)))
    ((s start end) 
     (let* ((len (- end start))
            (v (make-vector len)))
       (string->vector/itr! v s 0 start end)
       v))))
