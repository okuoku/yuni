(library (r7c-report misc case-lambda)
         (export case-lambda)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax letcore)
                 (r7c core apply)
                 (r7c core error)
                 (r7c heap listloop)
                 (r7c heap fixnum))

;; Took from 7.3 Derived expression types

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (params body0 ...) ...)
     (lambda args
       ($let/core ((len ($fx-length args)))
         (let-syntax
           ((cl (syntax-rules ::: ()
                  ((cl)
                   (error "no matching clause"))
                  ((cl ((p :::) . body) . rest)
                   (if ($fx= len ($fx-length '(p :::)))
                     (apply (lambda (p :::)
                              . body)
                            args)
                     (cl . rest)))
                  ((cl ((p ::: . tail) . body)
                       . rest)
                   (if ($fx>= len ($fx-length '(p :::)))
                     (apply
                       (lambda (p ::: . tail)
                         . body)
                       args)
                     (cl . rest))))))
           (cl (params body0 ...) ...)))))))

)
