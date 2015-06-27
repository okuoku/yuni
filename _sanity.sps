(import (yuni scheme)
        (scheme time)
        (scheme complex)
        (scheme inexact)
        (scheme write)
        (scheme lazy)
        (yuni testing testeval)
        (yuni async) (yuni core) 
        ; FIXME: Disable shorten library for now...
        ; (yuni base shorten)
        (yuni base match)
        (yuni core)
        (yuni miniobj minidispatch))

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
              (reverse failed-forms))))

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

;(check-equal 10 ((^a (+ 1 a)) 9))
;(check-equal 10 ((^ (form) (+ 2 form)) 8))
(check-equal 10 (match '(1 10 11) ((a b c) b)))

(let-values (((ex f?) (testeval 111 '((yuni scheme) (scheme time)))))
            (check-equal ex 111)
            (check-equal #t (not (failure? f?))))

(let-values (((ex f?) (testeval 111 '((only (yuni scheme) define)))))
            (check-equal ex 111)
            (check-equal #t (not (failure? f?))))

(let-values (((ex f?) (testeval 111 '((except (yuni scheme) define)))))
            (check-equal ex 111)
            (check-equal #t (not (failure? f?))))

(let-values (((ex f?) (testeval 'cons2 '((rename (yuni scheme) (cons cons2))))))
            (check-equal #t (procedure? ex))
            (check-equal #t (not (failure? f?))))

(let-values (((ex f?) (testeval 222 '((NEVERLAND)))))
            (check-equal #t (failure? f?)))

(define* testtype (entry-a entry-b))
(define* testtype2 (entry-a entry-b))

(define testobj0 (make testtype (entry-a 10)))

(begin
  (check-equal #t (is-a? testobj0 testtype))
  (check-equal #f (is-a? testobj0 testtype2))
  (check-equal 10 (~ testobj0 'entry-a))
  (~ testobj0 'entry-a := 1)
  (check-equal 1 (~ testobj0 'entry-a))
  (~ testobj0 'entry-b := 2)
  (check-equal 2 (~ testobj0 'entry-b))
  (touch! testobj0
    (entry-a 'a)
    (entry-b 'b))
  (let-with testobj0 (entry-a entry-b)
    (check-equal 'a entry-a)
    (check-equal 'b entry-b)))

(define (testfunc . param)
  (match param
         (('ref slot obj)
          (check-equal 'testme slot)
          (cdr obj))
         (('set! slot obj v)
          (check-equal 'testme slot)
          (set-cdr! obj v))))

(define-minidispatch-class testclass testfunc)

(define obj0 (make-minidispatch-obj testclass (cons #t #t)))

(~ obj0 'testme := "hoge")
(check-equal "hoge" (~ obj0 'testme))

(let-with obj0 (testme)
  (check-equal "hoge" testme))

(check-equal #t (is-a? obj0 testclass))

(check-finish)
