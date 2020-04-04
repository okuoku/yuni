(define (string->list/itr acc s cur start end)
  (if (= end cur)
    (reverse acc)
    (string->list/itr (cons (string-ref s cur) acc) s (+ 1 cur) start end)))

(define string->list
  (case-lambda
    ((s) (string->list s 0 (string-length s)))
    ((s start) (string->list s start (string-length s)))
    ((s start end) 
     (let ((len (- end start)))
       (string->list/itr '() s 0 start end)))))
