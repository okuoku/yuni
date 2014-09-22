(library (yunife logging)
         (export PCK)
         (import (yuni scheme))

(define ERRPORT current-error-port)
(define (PCK . obj)
  (if #t  ;; %verbose
    (begin
      (if #t ;; (not DEBUGGING)
        (begin
          (display "-> " (ERRPORT))
          (for-each (lambda (e)
                      (write e (ERRPORT))
                      (display " " (ERRPORT)))
                    obj)
          (newline (ERRPORT)))))))

)
