(import (yuni scheme)
        (yunivm heap hostbridge)
        (yunivm heap fake coreops))

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


(define-syntax checkheap
  (syntax-rules ()
    ((_ form)
     (let* ((fakeheap (make-coreops-fake))
            (fakebridge (make-hostbridge fakeheap))
            (fake-host->target (fakebridge 'TARGET))
            (fake-target->host (fakebridge 'HOST)))
       (let ((fake (fake-target->host (fake-host->target form))))
        (check-equal fake form))))))

(checkheap '())
(checkheap '(()))
(checkheap '(123 456))
(checkheap 'a)
(checkheap #\a)
(checkheap '(a b c d))
(checkheap '#(a b c d))
(checkheap "abcd")
(checkheap '(a "c" 1 #\a))
(checkheap '(#t #f "abc" "" #()))
(define theBv (bytevector 1 2 3 4))
(define theBv0 (bytevector))
(checkheap theBv)
(checkheap theBv0)

(check-finish)
