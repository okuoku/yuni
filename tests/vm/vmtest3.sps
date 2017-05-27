(import (yuni scheme)
        (yunivm util simplerunner))

(define test-counter 0)
(define success-counter 0)
(define failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display success-counter)
  (display "/")
  (display test-counter)
  (display " passed.") (newline)
  (unless (null? failed-forms)
    (newline)
    (display "Failed: ")
    (newline)
    (for-each (lambda x
                (display "    ")
                (write x)
                (newline))
              (reverse failed-forms)))
  (flush-output-port (current-output-port))
  (exit (if (null? failed-forms) 0 1)))


(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       (set! test-counter (+ 1 test-counter))
       (let ((e form))
        (cond ((equal? obj e)
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons 'form failed-forms)))))))))

(define-syntax check-scm
  (syntax-rules ()
    ((_ src expected)
     (let* ((runner (new-simplerunner))
            (code (simplerunner/expand-program runner 'src))
            (ir (simplerunner/treeir-compile runner code)))
       (set! test-counter (+ 1 test-counter))
       ;(display (list 'IR: ir)) (newline)
       (call-with-values (lambda () (simplerunner/treeir-run runner ir))
                         (lambda vals
                           (cond
                             ((equal? vals 'expected)
                              (set! success-counter (+ 1 success-counter)))
                             (else
                               (set! failed-forms (cons
                                                    (list
                                                      (list 'Scm: 'scm)
                                                      (list 'IR: ir)
                                                      (list 'Exp: 'expected)
                                                      (list 'Act: vals))
                                                    failed-forms))))))))))

(check-scm
  ((import (yuni scheme))
   (define (a) 10)
   (a))
  (10))

(check-scm
  ((import (yuni scheme) (yuni miniread reader))
   (define (a) 10)
   (a))
  (10))

(check-finish)
