(library (yunitest mini)
         (export check-finish
                 check-equal)
         (import (yuni scheme))

(define %%yunitest-mini-test-counter 0)
(define %%yunitest-mini-success-counter 0)
(define %%yunitest-mini-failed-forms '())

(define (check-finish)
  (display "Test: ")
  (display %%yunitest-mini-success-counter)
  (display "/")
  (display %%yunitest-mini-test-counter)
  (display " passed.\n")
  (unless (null? %%yunitest-mini-failed-forms)
    (display "\nFailed: \n")
    (for-each (lambda x
                (display "    ")
                (write x)
                (display "\n")
                ;; YuniVM: result convertion workaround
                #t)
              (reverse %%yunitest-mini-failed-forms)))
  ;; FIXME: Restore this later
  ;(flush-output-port (current-output-port))
  (exit (if (null? %%yunitest-mini-failed-forms) 0 1)))

(define (%%yunitest-mini-test-counter++)
  (set! %%yunitest-mini-test-counter (+ 1 %%yunitest-mini-test-counter)))

(define (%%yunitest-mini-success-counter++)
  (set! %%yunitest-mini-success-counter
    (+ 1 %%yunitest-mini-success-counter)))

(define (%%yunitest-mini-proc-fail! frm)
  (set! %%yunitest-mini-failed-forms 
    (cons frm
          %%yunitest-mini-failed-forms)))

(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (%%yunitest-mini-test-counter++)
       (let ((e form))
        (cond ((equal? obj e)
               (%%yunitest-mini-success-counter++))
              (else
                (%%yunitest-mini-proc-fail! 'form))))))))
         
)
