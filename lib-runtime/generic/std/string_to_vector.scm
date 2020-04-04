(define (string->vector/itr! v s cur start end)
  (unless (= end cur)
    (vector-set! v cur (string-ref s cur))
    (string->vector/itr! v s (+ 1 cur) start end)))

(define string->vector
  (case-lambda
    ((s) (string->vector s 0 (string-length s)))
    ((s start) (string->vector s start (string-length s)))
    ((s start end) 
     (let* ((len (- end start))
            (v (make-vector len)))
       (string->vector/itr! v s 0 start end)
       v))))
