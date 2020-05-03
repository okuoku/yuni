(library (yuni io leb128)
         (export
           ;; Unsigned
           leb128-put
           leb128-get)
         (import (yuni scheme))

;;

(define (leb128-put port i)
  (when (negative? i)
    (error "Negative number"))
  (unless (integer? i)
    (error "non-integer"))

  (let loop ((acc i))
   (cond
     ((< acc 128) ;; Term
      (write-u8 acc port))
     (else
       (let* ((d (exact (floor (/ acc 128))))
              (r (- acc (* d 128))))
         (write-u8 (+ r 128) port)
         (loop d))))))

(define (leb128-get port)
  (let loop ((acc 0)
             (mult 1)
             (b (read-u8 port)))
    (when (eof-object? b)
      (error "Premature end-of-file"))
    (cond
      ((< b 128) ;; Term
       (+ (* b mult) acc))
      (else
        (loop (+ (* (- b 128) mult) acc) (* mult 128) (read-u8 port))))))
         
)
