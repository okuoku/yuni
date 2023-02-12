(library (r7c-report misc lazy)
         (export delay-force make-promise delay force promise?)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c syntax lambda)
                 (r7c syntax letcore)
                 (r7c syntax unless)
                 (r7c heap pair)
                 (r7c heap list))

;; Took from 7.3 Derived expression types
;;    let => let/core
(define-syntax delay-force
  (syntax-rules ()
    ((delay-force expression)
     (make-promise '#f (lambda () expression)))))

(define-syntax delay
  (syntax-rules ()
    ((delay expression)
     (delay-force (make-promise '#t expression)))))

(define make-promise
  (lambda (done? proc)
    (list (cons done? proc))))

(define (force promise)
  (if (promise-done? promise)
    (promise-value promise)
    ($let/core ((promise* ((promise-value promise))))
      (unless (promise-done? promise)
        (promise-update! promise* promise))
      (force promise))))

(define promise-done?
  (lambda (x) (car (car x))))
(define promise-value
  (lambda (x) (cdr (car x))))

(define promise-update!
  (lambda (new old)
    (set-car! (car old) (promise-done? new))
    (set-cdr! (car old) (promise-value new))
    (set-car! new (car old))))

;; promise? implementation is not included 
(define (promise? x) (pair? x))
)
