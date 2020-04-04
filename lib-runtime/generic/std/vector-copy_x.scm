(define (vector-copy!/itr+ to at from start end)
  (unless (= start end)
    (vector-set! to at (vector-ref from start))
    (vector-copy!/itr+ to (+ 1 at) from (+ 1 start) end)))

(define (vector-copy!/itr- to at from start end)
  (vector-set! to at (vector-ref from end))
  (unless (= start end)
    (vector-copy!/itr- to (- at 1) from start (- end 1))))

(define vector-copy!
  (case-lambda
    ((to at from)
     (vector-copy! to at from 0 
                   (min (- (vector-length to) at) (vector-length from))))
    ((to at from start)
     (vector-copy! to at from start 
                   (min (+ start (- (vector-length to) at))
                        (vector-length from))))
    ((to at from start end)
     (unless (= start end)
       (cond
         ((and (eq? from to)
               (< start at end))
          (vector-copy!/itr- to (+ (- end start) at) from start (- end 1)))
         (else
           (vector-copy!/itr+ to at from start end)))))))
