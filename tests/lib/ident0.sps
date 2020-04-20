(import (yuni scheme)
        (yunitest mini)
        (yuni compat ident))

(define (getname)
  (let loop ((l (command-line)))
   (and (pair? l)
        (if (string=? "-IMPLNAME" (car l))
          (cadr l)
          (loop (cdr l))))))

(check-equal (ident-impl) (string->symbol (getname)))

(check-finish)
