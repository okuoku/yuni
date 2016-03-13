(library (r7c-report binding-construct let)
         (export let let*)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax letcore)
                 (r7c syntax lambda)
                 (r7c-report binding-construct letrec))


;; Took from 7.3 Derived expression types
;;    lambda => $let/core
(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ($let/core ((name val) ...) body1 body2 ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...) body1 body2 ...) 
       (let ((name1 val1))
         (let* ((name2 val2) ...)
           body1 body2 ...)))))
)
