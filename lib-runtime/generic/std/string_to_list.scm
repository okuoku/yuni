(define (string->list/itr acc s start end)
  (if (= start end)
    (reverse acc)
    (string->list/itr (cons (string-ref s start) acc) s (+ 1 start) end)))

(define string->list
  (case-lambda
    ((s) (string->list s 0 (string-length s)))
    ((s start) (string->list s start (string-length s)))
    ((s start end) 
     (string->list/itr '() s start end))))
