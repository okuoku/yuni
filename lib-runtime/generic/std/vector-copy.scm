(define vector-copy
  (case-lambda
    ((v) (vector-copy v 0 (vector-length v)))
    ((v start) (vector-copy v start (vector-length v)))
    ((v start end) 
     (let ((out (make-vector (- end start))))
      (vector-copy! out 0 v start end)
      out))))
