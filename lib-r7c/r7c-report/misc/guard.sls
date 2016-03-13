(library (r7c-report misc guard)
         (export guard else =>)
         (import (r7c-system core)
                 (r7c-system synrules)
                 (r7c-system auxsyntax)
                 (r7c core apply)
                 (r7c core callcc)
                 (r7c core exception)
                 (r7c syntax lambda)
                 (r7c syntax letcore))

;; Took from 7.3 Derived expression types


(define-syntax guard
  (syntax-rules ()
    ((guard (var clause ...) e1 e2 ...)
     ((call/cc
        (lambda (guard-k)
          (with-exception-handler
            (lambda (condition)
              ((call/cc
                 (lambda (handler-k)
                   (guard-k
                     (lambda ()
                       ($let/core ((var condition))
                         (guard-aux
                           (handler-k
                             (lambda ()
                               (raise-continuable condition)))
                           clause ...))))))))
            (lambda ()
              (call-with-values
                (lambda () e1 e2 ...)
                (lambda args
                  (guard-k
                    (lambda ()
                      (apply values args)))))))))))))

(define-syntax guard-aux
  (syntax-rules (else =>)
    ((guard-aux reraise (else result1 result2 ...))
     (begin result1 result2 ...))
    ((guard-aux reraise (test => result))
     ($let/core ((temp test))
       (if temp
         (result temp)
         reraise)))
    ((guard-aux reraise (test => result)
                clause1 clause2 ...)
     ($let/core ((temp test))
       (if temp
         (result temp)
         (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test))
     (or test reraise))
    ((guard-aux reraise (test) clause1 clause2 ...)
     ($let/core ((temp test))
       (if temp
         temp
         (guard-aux reraise clause1 clause2 ...))))
    ((guard-aux reraise (test result1 result2 ...))
     (if test
       (begin result1 result2 ...)
       reraise))
    ((guard-aux reraise
                (test result1 result2 ...)
                clause1 clause2 ...)
     (if test
       (begin result1 result2 ...)
       (guard-aux reraise clause1 clause2 ...)))))

)
