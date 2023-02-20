(library (r7c-report misc parameterize)
         (export parameterize make-parameter)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c heap list)
                 (r7c syntax cond)
                 (r7c syntax letcore)
                 (r7c core dynamic-wind)
                 )


;; FIXME: 
(define <param-set!> (list '"unique"))
(define <param-convert> (list '"unique"))

;; Took from 7.3 Derived expression types

(define (make-parameter init . o)
      (let* ((converter
              (if (pair? o) (car o) (lambda (x) x)))
             (value (converter init)))
        (lambda args
          (cond
           ((null? args)
            value)
           ((eq? (car args) <param-set!>)
            (set! value (cadr args)))
           ((eq? (car args) <param-convert>)
            converter)
           (else
            (error "bad parameter syntax"))))))

(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ("step")
                   ((param value p old new) ...)
                   ()
                   body)
     ($let/core ((p param) ...)
       ($let/core ((old (p)) ...
                  (new ((p <param-convert>) value)) ...)
         (dynamic-wind
           (lambda () (p <param-set!> new) ...)
           (lambda () . body)
           (lambda () (p <param-set!> old) ...)))))
    ((parameterize ("step")
                   args
                   ((param value) . rest)
                   body)
     (parameterize ("step")
                   ((param value __1 __2 __3) . args)
                   rest
                   body))
    ((parameterize ((param value) ...) . body)
     (parameterize ("step")
                   ()
                   ((param value) ...)
                   body))))


)
