(library (r6rs-common-yuni compat macro primitives0)
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
                              (syntax none)
                              (string->symbol
                                (apply string-append
                                       (conv (syntax->datum (syntax (x ...)))))))))
         (syntax 
           (lambda (h)
             (syntax-case h ()
               ((out param)
                (with-syntax ((name (datum->syntax (syntax out) 'total)))
                  (syntax 
                    (let-syntax ((b (lambda (y)
                                      (syntax-case y ()
                                        ;; Definition
                                        ((_ obj)
                                         (datum->syntax (syntax out) 
                                                        (syntax->datum 
                                                          (syntax obj))))
                                        ;; Binding
                                        ((_ binding obj)
                                         ;; Strip syntactic information
                                         (let ((bind (syntax->datum 
                                                       (syntax binding)))
                                               (prog (syntax->datum 
                                                       (syntax obj))))
                                           (datum->syntax
                                             (syntax out)
                                             `(letrec ((name ,bind))
                                                ,prog))))))))
                      (begin
                       (k b name param))))))))))))))

)
