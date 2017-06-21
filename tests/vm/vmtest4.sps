(import (yuni scheme))

(define test-counter 0)
(define success-counter 0)
(define failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display success-counter)
  (display "/")
  (display test-counter)
  (display " passed.")(newline)
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

;; lists

(check-equal 'a (append 'a))
(check-equal '(10 20) (append '(10) '(20)))
(check-equal '(10 20) (append '(10) '() '(20)))
(check-equal '(10 20) (append '(10) '() '() '(20)))
(check-equal '(10 . a) (append '(10) 'a))
(check-equal '(10 20 . a) (append '(10) '(20) 'a))
(check-equal '(10 20 30 . a) (append '(10) '(20) '(30) 'a))

(check-finish)
