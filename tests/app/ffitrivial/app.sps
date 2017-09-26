(import (yuni scheme)
        (yuni ffi abi abiv0-runtime)
        (yuni compat ffi primitives)
        (yuni compat bitwise primitives)

        ;; Generated stubs
        (yunistub testing_trivial-constants))

;; Tentative module runtime

(define (lookup-forward0 obj)
  (and (pair? obj)
       (let ((a (car obj))
             (d (cdr obj)))
         (if (eq? (car a) 'forward-0)
           (cdr a)
           (lookup-forward0 d)))))

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


(define mod (yuniffi-module-load "yunistub_test_primitives"))
(define xmod (yuniffi-module-load "NEVERLAND"))
(define trivial (yuniffi-module-load "yunistub_testing_trivial"))

;; xmod should never be found
(check-equal #f xmod)

(check-equal #t (and trivial #t))
(when trivial
  (let* ((tbl (yuniffi-abiv0-lookup/constants trivial
                                              "testing_trivial"))
         (lis (yuniffi-abiv0-get-table tbl)))
    (check-equal #t (list? lis))
    (check-equal -1234 MINUS_CONST_1)
    ;(check-equal 0.5 REAL_1)
    ;(check-equal 0.5 REAL_2)
    (check-equal #f CONST_1)
    (check-equal 1234 CONST_2)
    (check-equal #f UNDEFINED)
    ;(for-each (lambda (e) (write e)(newline)) lis)
    (let* ((echo (lookup-forward0 testecho_intecho))
           (in (make-bytevector (* 8 1)))
           (out (make-bytevector (* 8 1))))
      (check-equal #t (and echo #t))
      (when echo
        (let ((echofunc (yuniffi-nccc-ptr->callable echo)))
         (let loop ((idx 0)
                    (chk '())
                    (cur '()))
           (define (call1 num)
             (bv-write/u64! in 0 num)
             (bv-write/u64! out 0 9999)
             (yuniffi-nccc-call echofunc in 0 1 out 0 1)
             (bv-read/u64 out 0))
           (cond
             ((= idx 4096)
              (check-equal chk cur))
             (else
               (let ((n (call1 idx)))
                (loop (+ idx 1)
                      (cons idx chk)
                      (cons n cur)))))))))
    (let* ((callcallback (lookup-forward0 test_callcallback)))
     (check-equal #t (and callcallback #t))
     (when callcallback
       (let ((proc (yuniffi-nccc-ptr->callable callcallback))
             (in (make-bytevector (* 8 4)))
             (out (make-bytevector (* 8 1)))
             (func (yuniffi-nccc-proc-register
                     (lambda ab
                       (display "called.\n")
                       (write ab)
                       (newline)))))
         (write (list 'CALLBACK func))
         (newline)
         (bv-write/w64ptr! in 0 func)
         (bv-write/u64! in 8 10)
         (bv-write/u64! in 16 20)
         (yuniffi-nccc-call proc in 0 3 out 0 1))))))

(check-finish)
