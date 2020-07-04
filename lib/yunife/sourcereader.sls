(library (yunife sourcereader)
         (export read-source)
         (import (yuni scheme))

;;

(define (read-source pth)
  (call-with-input-file
    pth        
    (lambda (p) 
      (define (itr cur)
        (let ((r (read p)))
         (if (eof-object? r)
             (reverse cur)
             (itr (cons r cur)))))
      (itr '()))))

)
