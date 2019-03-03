(import (yuni scheme)
        (yunitest mini))

(check-equal 'a `a)
(check-equal `a `a)
(check-equal #\a `#\a)
(check-equal (list "a") `("a"))
(let ((a "a")
      (aa "a")
      (b "b"))
  (check-equal (list "a" "a") `(,a ,aa))
  (check-equal (list "a" "b") `(,a ,b)))

(let ((a (list "a" "b" "c")))
 (check-equal (list "0" "a" "b" "c") `("0" ,@a)))

;; R7RS
(check-equal '(list 3 4) `(list ,(+ 1 2) 4))
(check-equal '(list a (quote a)) (let ((name 'a)) `(list ,name ',name)))
(check-equal '(a 3 4 5 6 b) `(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b))
(check-equal '((foo 7) . cons) `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons))))
;(check-equal '#(10 5 2 4 3 8) `#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)) ;; S7
;; FIXME: Last example says `@baz` which is not valid Scheme symbol for us

;; R7RS 
(check-equal '(a `(b ,(+ 1 2) ,(foo 4 d) e) f)
             `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f))

(check-equal '(a `(b ,x ,'y d) e)
             (let ((name1 'x)
                   (name2 'y))
               `(a `(b ,,name1 ,',name2 d) e)))

(check-finish)
