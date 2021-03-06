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

(define-syntax check-scm0
  (syntax-rules ()
    ((_ sel src expected)
     (let* ((runner (if sel (new-simplerunner/fakeheap) (new-simplerunner)))
            (code (simplerunner/expand-program runner 'src))
            (ir (begin
                  ;(pp code)
                  (simplerunner/treeir-compile runner code))))
       (set! test-counter (+ 1 test-counter))
       ;(pp ir)
       ;(display (list 'IR: ir)) (newline)
       (call-with-values (lambda () (simplerunner/treeir-run runner ir))
                         (lambda vals
                           (cond
                             ((equal? vals 'expected)
                              (set! success-counter (+ 1 success-counter)))
                             (else
                               (set! failed-forms (cons
                                                    (list
                                                      (list 'Sel: 'sel)
                                                      (list 'Scm: 'code)
                                                      ;(list 'IR: ir)
                                                      (list 'Exp: 'expected)
                                                      (list 'Act: vals))
                                                    failed-forms))))))))))

(define-syntax check-scm
  (syntax-rules ()
    ((_ src expected)
     (begin
       ;(check-scm0 #f src expected)
       (check-scm0 #t src expected)))))

(check-scm
  ((import (yuni scheme) (yuni core))
   (define* testtype (a b))
   (define obj0 (make testtype (a 10)))
   (~ obj0 'a))
  (10))

(check-scm
  ((import (yuni scheme))
   (define (a) 10)
   (a))
  (10))

(check-scm
  ((import (yuni scheme) (yuni miniread reader))
   (define-syntax conv
     (syntax-rules ()
       ((_ objs ...)
        (list
          (do-conv objs)
          ...))))
   (define (do-conv str)
     (let* ((bv (string->utf8 str))
            (obj1 (utf8-read bv)))
       obj1))
   (conv
     "10"
     "#| # |# hoge"
     "'abc"
     ",()"
     ",(,abc)"))
  (((10) (hoge) ('abc) (,()) (,(,abc)))))

(check-scm
  ((import (yuni scheme))
   (list
     (append)
     (append 'a)
     (append '(10) '(20))
     (append '(10) '(20) '())
     (append '() '(10) '(20) '())
     (append '(10) 'a)
     )
   )
  ((
    ;; (append)
    ()
    ;; (append 'a)
    a
    (10 20)
    (10 20)
    (10 20)
    (10 . a))))

(check-finish)
