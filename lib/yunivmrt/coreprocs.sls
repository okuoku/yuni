(library (yunivmrt coreprocs)
         (export
           ;; (r7c heap boolean)
           not
           ;; (r7c heap pair)
           pair?
           null?
           cons
           car
           cdr
           caar
           cadr
           cdar
           cddr
           set-car!
           set-cdr!
           ;; (r7c heap list)
           list
           append
           ;; (r7c heap vector)
           list->vector
           vector->list
           ;; (r7c heap undefined)
           $undefined ;; for letrec
           ;; (r7c heap listloop)
           memv
           $fx-length
           ;; (r7c heap fixnum)
           $fx>=
           $fx=
           ;; (r7c core values)
           values
           call-with-values
           ;; (r7c core apply)
           apply
           ;; (r7c core error)
           error
           ;; (r7c core callcc)
           call/cc
           call-with-current-continuation
           ;; (r7c core exception)
           with-exception-handler
           raise
           raise-continuable
           )
         (import (yunifake bogus)
                 (yunivmrt coresyntax))



;; NB: These are defined as syntax because basic procs called
;;     from basic syntaxes cannot be exported.
(define-syntax $undefined
  (syntax-rules ()
    ((_) (lambda () (if #f #f)))))

(define-syntax $fx-length
  (syntax-rules ()
    ((_ a) (length a))))

(define-syntax $fx>=
  (syntax-rules ()
    ((_ a b) (>= a b))))

(define-syntax $fx=
  (syntax-rules ()
    ((_ a b) (= a b))))

(define-primitive-names/yunifake
  not
  pair?
  null?
  cons
  car
  cdr
  caar
  cadr
  cdar
  cddr
  set-car!
  set-cdr!
  list
  append
  list->vector
  vector->list
  memv
  values
  call-with-values
  apply
  error
  call/cc
  call-with-current-continuation
  with-exception-handler
  raise
  raise-continuable)
         
)
