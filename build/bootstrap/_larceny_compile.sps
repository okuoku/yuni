(import (rnrs)
        (primitives file-modification-time)
        (larceny compiler))

(define file (caddr (command-line)))
(define fasl 
  (let ((len (- (string-length file) 4)))
   (string-append (substring file 0 len) ".slfasl")))

(define retry-limit 100)

(define (try)
  (compile-file file)
  (unless (file-exists? fasl)
    (assertion-violation 'try "Cannot create FASL" file fasl))
  (let ((a (file-modification-time file))
        (b (file-modification-time fasl)))
    ;(display (list 'file: a)) (newline)
    ;(display (list 'fasl: b)) (newline)
    (when (equal? a b)
      (display (list "Retry..." a)) (newline)
      (set! retry-limit (- retry-limit 1))
      (unless (positive? retry-limit)
        (assertion-violation 'try "Too many retries"))
      (try))))

(try)

