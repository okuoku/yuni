(library (yuni compat macro primitives0)
         (export syntax-inject)
         (import (for (rnrs) run (meta -1)))

(define-syntax syntax-inject
  (lambda (stx)
    (define (conv lis)
      (map (lambda (s)
             (cond ((symbol? s) (symbol->string s))
                   ((string? s) s)
                   (syntax-violation
                     #f
                     "Invalid object as part of identifier" s )))
           lis))

    (syntax-case stx ()
      ((_ (x ...) k)
       (with-syntax ((total (datum->syntax
                              #'none
                              (string->symbol
                                (apply string-append
                                       (conv (syntax->datum #'(x ...))))))))
         #'(lambda (h)
             (syntax-case h ()
               ((out param)
                (with-syntax ((name (datum->syntax #'out 'total)))
                  #`(let-syntax ((b (lambda (y)
                                      (syntax-case y ()
                                        ((_ binding obj)
                                         ;; Strip syntactic information
                                         (let ((bind (syntax->datum #'binding))
                                               (prog (syntax->datum #'obj)))
                                           (datum->syntax
                                             #'out
                                             `(letrec ((name ,bind))
                                                ,prog))))))))
                      (begin
                       (k b name param))))))))))))
         
)
