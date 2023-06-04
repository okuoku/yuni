(import (yuni scheme)
        (yuni compat ident)
        (yunitest mini))

(define (str-nonterm) 
  (case (ident-impl)
    ((gambit foment) #t)
    (else #f)))

(define (str-term) 
  (case (ident-impl)
    ((gambit cyclone foment ribbon) #t)
    (else #f)))

(define (checkans s)
  (cond
    ((and (string? s) (string=? s ""))
     #f)
    ((eof-object? s)
     #t)
    (else "Unexpected response!")))

(check-equal (str-nonterm)
             (checkans
               (let ((p (open-input-string "xxxx")))
                (read-string 0 p))))

(check-equal (str-term)
             (checkans
               (let ((p (open-input-string "")))
                (read-string 0 p))))

(check-finish)
