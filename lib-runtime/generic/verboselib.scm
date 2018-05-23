(define yuni/%verbose #t)
(define ERRPORT current-error-port)
(define (PCK . obj)
  (cond
    (yuni/%verbose
      (display "-> " (ERRPORT))
      (for-each (lambda (e)
                  (write e (ERRPORT))
                  (display " " (ERRPORT)))
                obj)
      (newline (ERRPORT)))))


