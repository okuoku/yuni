(library (A)
         (export A stxA)
         (import (yuni scheme)
                 (yunitest mini))

(define-syntax stxA
  (syntax-rules ()
    ((_ sym)
     (begin
       (display "stxA: Symbol ")
       (display 'sym)
       (display " value is: ")
       (write sym)
       (display "\n")))))

(define-syntax stxTest
  (syntax-rules ()
    ((_ sym ...)
     (list 'sym ...))))

(define (test)
  (display "This is `test` in library (A)\n")
  ;; Test aux syntax (qq)
  (check-equal (list 'a 'b 10 "string")
               (let ((rest (list 10 "string"))
                     (c 'b))
                `(a ,c ,@rest)))
  ;; Test aux syntax (else)
  (check-equal "ok"
               (cond
                 ((= 0 1) 'no)
                 (else "ok")))
  (check-equal "ok"
               (cond
                 ((= 0 0) "ok")
                 (else 'ng)))
  (check-equal "ok"
               (case 'n
                 ((a b) 'no)
                 (else "ok")))
  ;; Test aux syntax (dots)
  (check-equal '(a b c)
               (stxTest a b c))
  #|
  ;; Test aux syntax (arrow)
  (check-equal "ok"
               (cond
                 ((stxTest a b) => (lambda (x)
                                     (and (equal? '(a b)
                                                  x)
                                          "ok")))
                 (else "no")))
  |#
  #|
  ;; Test R7RS case
  (check-equal "ok"
               (case 'n
                 ((x y n) => (lambda (p) (and (eq? 'n p)
                                              "ok")))
                 (else 'no)))
  (check-equal "ok"
               (case 'n
                 ((x y) => (lambda (p) (and (eq? 'x p)
                                            "no")))
                 (else => (lambda (p) (and (eq? 'n p)
                                           "ok")))))
  |#
  'A)         

(define (A)
  (let ((a (test)))
   (unless (eq? 'A a)
     (error "something wrong (A)" a))))         


)
