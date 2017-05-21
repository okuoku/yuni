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
    ((_ scm expected)
     (let* ((runner (new-simplerunner))
            (ir (simplerunner/treeir-compile runner 'scm)))
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
  (#f)
  (#f))

(check-scm
  (#t)
  (#t))

(check-scm
  (123)
  (123))

(check-scm
  ("abc")
  ("abc"))

(check-scm
  (#\c)
  (#\c))

(check-scm
  ('())
  (()))

(check-scm
  ('master)
  (master))

(check-scm
  ((begin 1 2 3))
  (3))

(check-scm
  ((if #t 123))
  (123))

(check-scm
  ((if #f 123 456))
  (456))

(check-scm
  ((let ((f (lambda (a b c) c)))
    (f 1 2 3)))
  (3))

(check-scm
  ((let ((f (lambda (a b . c) c)))
    (f 1 2 3 4)))
  ((3 4)))

(check-scm
  ((let ((f (lambda (a b . c) c)))
    (f 1 2 3)))
  ((3)))

(check-scm
  ((let ((f (lambda (a b . c) c)))
    (f 1 2)))
  (()))

(check-scm
  ((let ((f (lambda a a)))
    (f 1 2 3)))
  ((1 2 3)))

(check-scm
  ((let ((g (lambda (a b) b))
         (f (lambda (a b) (g a b))))
    (f 1 2)))
  (2))

(check-scm
  ((+ 2 3))
  (5))

(check-finish)
