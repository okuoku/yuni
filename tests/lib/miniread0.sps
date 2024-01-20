(import (yuni scheme)
        (yuni miniread reader)
        (yunitest mini))

;;

(define (port->sexp p)
  (define (itr cur)
    (let ((r (read p)))
     (if (eof-object? r)
       (reverse cur)
       (itr (cons r cur))) ))
  (itr '()))

(define (checkobj str obj)
  (let* ((bv (string->utf8 str))
         (obj1 (utf8-read bv)))
    (check-equal obj1 obj)))
(define (check str)
  (let* ((p (open-input-string str))
         (obj0 (port->sexp p)))
    (checkobj str obj0)))
(define-syntax check2
  (syntax-rules ()
    ((_ obj ...)
     (let* ((p (open-output-string))
            (gen (lambda (e) (write e p) (write-char #\space p))))
       (for-each gen '(obj ...))
       (let* ((str (get-output-string p))
              (bv (string->utf8 str))
              (obj1 (utf8-read bv)))
         ;(write (list 'str: str))(newline)
         ;(write (list 'bv: bv))(newline)
         ;(write (list 'obj: '(obj ...)))(newline)
         ;(write (list 'obj1: obj1 ))(newline)
         ;(newline)
         (check-equal obj1 '(obj ...)))))))

(check "#| # |# hoge")
(check "...")
(check "(...)")
(check "(a . b)")
(check "(a b . c)")
(check2 a)
(check2 a b c d)
(check2 #\a "hoge")
(check2 "\"")
(check2 "hoge" "hoge")
(check2 "hoge" fuga "hoge")
(check2 ("hoge\"" fuga "\"hoge")) ;; FIXME: Same as the case just below
;(check "\"hoge \\n hoge\"") ;; FIXME: WHY??
(checkobj "`(hoge ,fuga)" (list '(quasiquote (hoge (unquote fuga))))) ;; S7
(checkobj "`(hoge ,@fuga)" (list '(quasiquote (hoge (unquote-splicing fuga))))) ;; S7
(check "a b c")
(check "#\\a")
;(check "#\\linefeed")
(checkobj "#;(hoge) fuga" (list 'fuga)) ;; S7
(check "#| hoge |# fuga")
(check ";; fuga\nhoge")
(check "(100 () (1 2 3) 100)")
(check "'abc")
(checkobj ",abc" (list '(unquote abc))) ;; S7
(checkobj ",()" (list '(unquote ()))) ;; S7
(checkobj ",(,abc)" (list '(unquote ((unquote abc))))) ;; S7
(checkobj ",(,@abc)" (list '(unquote ((unquote-splicing abc))))) ;; S7
(check "100\n")
(check "")
(check "100")
(check "(100 100)")
(check "(\"ABC\")")
(check "(100 \"ABC\")")
(check "#(100 100)")
(check "#()")
(check "(+ 1)")
(check "(- 1)")
(check "',name")
(check "`,name")
(check "',@name")
(check "`,@name")
(check "(',name)")
(check "(`,name)")
(check "(',@name)")
(check "(`,@name)")

(checkobj "#vu8(1 2 3 4)" (list (bytevector 1 2 3 4)))
(checkobj "#u8(1 2 3 4)" (list (bytevector 1 2 3 4)))
(checkobj "#u8()" (list (bytevector)))
(checkobj "#vu8(0)" (list (bytevector 0)))


(check-finish)
