#!r6rs
(library (yuni-runtime racket)
         (export %%internal-paste)
         (import (rnrs)
                 (only (racket) read-syntax syntax->list))

(define-syntax %%internal-paste
  (lambda (x)
    (syntax-case x ()
      ((here pth)
       (let ((pathname (syntax->datum #'pth)))
         (call-with-input-file 
           pathname
           (lambda (p)
             ;; FIXME: It strips syntactic information....
             (let ((stx (syntax->datum (read-syntax pathname p))))
               #`(begin #,@(datum->syntax #'here (cddddr stx)))))))))))
         
)
