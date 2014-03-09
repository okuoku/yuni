(library (yuni binary chat)
         (export make-message-waiter0)
         (import (rnrs))

(define (make-message-waiter0 str/bv cb) ;; => ^[b]
  ;; cb = ^[]
  (define offset 0)
  (define bytesv (list->vector (bytevector->u8-list
                                 (if (string? str/bv) 
                                   (string->utf8 str/bv) str/bv))))
  (define finish (vector-length bytesv))
  (define (wait b)
    (cond 
      ((= (vector-ref bytesv offset) b)
       (set! offset (+ offset 1)) 
       (when (= offset finish)
         (cb)
         (set! offset 0))) 
      (else
        (set! offset 0))))
  wait)

)
