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

;; list?
(check-equal #t (list? '(10)))
(check-equal #t (list? '(10 20 30)))
(check-equal #t (list? '()))
(check-equal #f (list? '(10 . 20)))
(check-equal #f (list? 10))
;; list
(check-equal '() (list))
(check-equal '(10) (list 10))
(check-equal '(10 20) (list 10 20))
;(check-equal '(10 20) (apply list 10 '(20))) ;; FIXME
;(check-equal '(10 20 30) (apply list 10 '(20 30))) ;; FIXME

;; append
(check-equal 'a (append 'a))
(check-equal '(10 20) (append '(10) '(20)))
(check-equal '(10 20) (append '(10) '() '(20)))
(check-equal '(10 20) (append '(10) '() '() '(20)))
(check-equal '(10 . a) (append '(10) 'a))
(check-equal '(10 20 . a) (append '(10) '(20) 'a))
(check-equal '(10 20 30 . a) (append '(10) '(20) '(30) 'a))
;; reverse
(check-equal '() (reverse '()))
(check-equal '(10) (reverse '(10)))
(check-equal '(10 20) (reverse '(20 10)))
(check-equal '(10 20 30) (reverse '(30 20 10)))
(check-equal '(30 (20 10 0) 20 10) (reverse '(10 20 (20 10 0) 30)))
;; memq
(check-equal '(a) (memq 'a '(a)))
(check-equal '(a b) (memq 'a '(a b)))
(check-equal '(b) (memq 'b '(a b)))
(check-equal #f (memq 'z '(a b)))
(check-equal #f (memq 'z '()))
;; assq
;; assv
;; make-list
(check-equal '() (make-list 0 'a))
(check-equal '(a) (make-list 1 'a))
(check-equal '(a a a a) (make-list 4 'a))
;; length
(check-equal 0 (length (make-list 0)))
(check-equal 300 (length (make-list 300)))
(check-equal 300 (length (make-list 300 #t)))
;; list-tail
;; list-ref
;; list-set!
(define (list-set!/check l k v) (list-set! l k v) l)
(check-equal '(10) (list-set!/check (list 20) 0 10))
(check-equal '(10 20 30) (list-set!/check (list 10 #f 30) 1 20))
(check-equal '(10 20 30) (list-set!/check (list 10 20 #f) 2 30))
;; list-copy
(check-equal 'a (list-copy 'a))
(check-equal '(10) (list-copy '(10)))
(check-equal '(10 . 20) (list-copy '(10 . 20)))
(check-equal '(10 20 30) (list-copy '(10 20 30)))
(check-equal '() (list-copy '()))

(check-finish)
