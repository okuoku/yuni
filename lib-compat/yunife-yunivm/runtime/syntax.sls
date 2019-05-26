(library (yunife-yunivm runtime syntax)
         (export
           and or unless
           case cond do)
         (import 
           ;; FIXME: No library required for core procedures
           ;; For define-syntax:
           (yunife-yunivm runtime let)
           (yunivm-core-syntax))

;; Took from 7.3 Derived expression types
;; Use __1 as binding
(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
       (test expr ...)
       command ...)
     (letrec
       ((__1
          (lambda (var ...)
            (if test
              (begin
                (if #f #f)
                expr ...)
              (begin
                command
                ...
                (__1 (do "step" var step ...)
                      ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

;; As we have native WHEN, use it instead of IF
(define-syntax unless
  (syntax-rules ()
    ((_ test body0 body1 ...)
     (when (not test)
       (begin body0 body1 ...)))))

;; Took from 7.3 Derived expression types
(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

;; Took from 7.3 Derived expression types
(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (if test1 test1 (or test2 ...)))))

;; Took from 7.3 Derived expression types
;; Use __1 as binding
(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((__1 test))
       (if __1 (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((__1 test))
       (if __1 
         (result temp)
         (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((__1 test))
       (if __1 temp
         (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
       (begin result1 result2 ...)
       (cond clause1 clause2 ...)))))

;; Took from 7.3 Derived expression types
;; Use __1 as binding
(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...) clauses ...)
     (let ((__1 (key ...)))
       (case __1 clauses ...)))
    ((case key (else => result))
     (result key))
    ((case key (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
       (begin result1 result2 ...)))
    ((case key ((atoms ...) => result))
     (if (memv key '(atoms ...))
       (result key)))
    ((case key ((atoms ...) => result) clause clauses ...)
     (if (memv key '(atoms ...))
       (result key)
       (case key clause clauses ...)))
    ((case key ((atoms ...) result1 result2 ...) clause clauses ...)
     (if (memv key '(atoms ...))
       (begin result1 result2 ...)
       (case key clause clauses ...)))))
         
)
