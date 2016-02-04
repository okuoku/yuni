;; FIXME: This is currently a pure-R6RS...
;;        We should be able to retain source information with
;;        source-properties
#!r6rs
(library (yuni-runtime guile)
         (export %%internal-paste)
         (import (rnrs))

(define-syntax %%internal-paste
  (lambda (x)
    (syntax-case x ()
      ((here pth)
       (let ((pathname (syntax->datum #'pth)))
         (call-with-input-file 
           pathname
           (lambda (p)
             ;; FIXME: It strips syntactic information....
             (let ((stx (read p)))
               #`(begin #,@(datum->syntax #'here (cddddr stx)))))))))))
         
)
