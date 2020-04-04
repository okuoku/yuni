(define string-copy
  (case-lambda
    ((s) (string-copy s 0 (string-length s)))
    ((s start) (string-copy s start (string-length s)))
    ((s start end) (list->string (string->list s start end)))))
