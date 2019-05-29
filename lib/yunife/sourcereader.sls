(library (yunife sourcereader)
         (export read-source)
         (import (yuni scheme))

;;

(define (read-source pth)
  (with-input-from-file
    pth        
    (lambda () 
      (define (itr cur)
        (let ((r (read (current-input-port))))
         (if (eof-object? r)
             (reverse cur)
             (itr (cons r cur)))))
      (itr '()))))

)
