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
        (yuni base dispatch)
        (yuni miniread reader)
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
              (reverse failed-forms)))
  (flush-output-port (current-output-port)))

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

;; (yuni core)

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

;; (yuni base dispatch)

(define dispatch0 (dispatch-lambda
                    (('pass1 x)
                     (check-equal x 1)
                     "OKAY")
                    (('pass1alt x)
                     (check-equal x 2)
                     "OKAYalt")
                    (('pass2-2 x y)
                     (check-equal x 2)
                     (check-equal y 2)
                     "OKAY")
                    (('passnone)
                     "OKAY")
                    ((pass str)
                     (check-equal #t (string? pass))
                     (check-equal #t (string? str))
                     "OKAY")))

(check-equal "OKAY" (dispatch0 'pass1 1))
(check-equal "OKAYalt" (dispatch0 'pass1alt 2))
(check-equal "OKAY" (dispatch0 'pass2-2 2 2))
(check-equal "OKAY" (dispatch0 'passnone))
(check-equal "OKAY" (dispatch0 "str" "str"))

;; (yuni miniread reader) and base reader

(define (equal-check-deep sexp0 sexp1)
  (define (comp ctx s0 s1)
    (cond
      ((pair? s0)
       (if (pair? s1)
         (and (comp (cons s0 ctx) (car s0) (car s1))
              (comp (cons s0 ctx) (cdr s0) (cdr s0)))
         (error "pair-unmatch!" s0 s1)))
      (else
        (let ((e (equal? s0 s1)))
         #|
         (when e
           (write (list 'MATCH: ctx s0 s1))(newline))
         |#
         (unless e
           (error "datum-unmatch!" ctx (list s0 s1)))
         e))))
  (check-equal #t (comp '() sexp0 sexp1)))

(define (port->sexp p)
  (define (itr cur)
    (let ((r (read p)))
     (if (eof-object? r)
       (reverse cur)
       (itr (cons r cur))) )) 
  (itr '()))

(define (file->sexp pth)
  (define p (open-input-file pth))
  (let ((obj (port->sexp p)))
   (close-port p)
   obj)) 

(define (textfile->bytevector pth)
  (define p (open-input-file pth))
  (define (itr cur)
    (let ((l (read-line p)))
     (if (eof-object? l)
       (string->utf8 cur)
       (itr (if (string=? "" cur) 
              l
              (string-append cur "\n" l))))))
  (itr ""))

(define (verify-file pth)
  (let ((x (file->sexp pth))
        (y (utf8-read (textfile->bytevector pth))))
    (equal-check-deep x y)))

(define yuni-compat-libs
  (begin
    (unless (file-exists? "_testing_liblist.txt")
      (error "_testing_liblist.txt was not found. Generate it with run/buildstub.sh first."))
    (let ((p (open-input-file "_testing_liblist.txt")))
     (define (itr cur)
       (let ((l (read-line p)))
        (if (eof-object? l) 
          cur
          (itr (cons l cur)))))
     (itr '()))))

(define test-files (append yuni-compat-libs '("_sanity.sps" "_ncccsanity.sps")))

(define (miniread-tests)
  (define (checkobj str obj)
    (define bv (string->utf8 str))
    (define obj1 (utf8-read bv))
    (check-equal obj1 obj))
  (define (check str)
    (define p (open-input-string str))
    (define obj0 (port->sexp p))
    (checkobj str obj0))
  (check "#| # |# hoge")
  ;(check "\"hoge \\n hoge\"") ;; FIXME: WHY??
  (check "`(hoge ,fuga)")
  (check "`(hoge ,@fuga)")
  (check "a b c")
  (check "#\\a")
  ;(check "#\\linefeed")
  (check "#;(hoge) fuga")
  (check "#| hoge |# fuga")
  (check ";; fuga\nhoge")
  (check "(100 () (1 2 3) 100)")
  (check "'abc")
  (check ",abc")
  (check ",()")
  (check ",(,abc)")
  (check ",(,@abc)")
  (check "100\n")
  (check "")
  (check "100")
  (check "(100 100)")
  (check "(\"ABC\")")
  (check "(100 \"ABC\")")
  (check "#(100 100)")
  (check "#()")

  (checkobj "#vu8(1 2 3 4)" (list (bytevector 1 2 3 4)))
  (checkobj "#u8(1 2 3 4)" (list (bytevector 1 2 3 4)))
  (checkobj "#u8()" (list (bytevector)))
  (checkobj "#vu8(0)" (list (bytevector 0)))
  )

(for-each verify-file test-files)
(miniread-tests)

(check-finish)
