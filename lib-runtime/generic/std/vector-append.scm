(define (vector-append . q)
  (define totallen 0)
  ;; Calc totallen
  (for-each (lambda (v) (set! totallen (+ totallen (vector-length v)))) q)
  (let ((out (make-vector totallen))
        (off 0))
    (for-each (lambda (v)
                (vector-copy! out off v)
                (set! off (+ off (vector-length v))))
              q)
    out))
