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
                (newline) 
                ;; YuniVM: result convertion workaround
                #t)
              (reverse failed-forms)))
  (flush-output-port (current-output-port))
  (exit (if (null? failed-forms) 0 1)))

(define-syntax check-equal
  (syntax-rules ()
    ((_ obj form)
     (begin
       ;(display (list 'obj 'form)) (newline)
       (set! test-counter (+ 1 test-counter))
       (let ((e form))
        (cond ((equal? obj e)
               (set! success-counter (+ 1 success-counter)))
              (else
                (set! failed-forms (cons 'form failed-forms)))))))))

;; Support macros
(define-syntax copytest
  (syntax-rules ()
    ((_ copy (copyproc! obj at ...))
     (let ((target (copy obj)))
      (copyproc! target at ...)
      target))))

;; boolean
(check-equal #t (boolean=? #t #t))
(check-equal #t (boolean=? #f #f))
(check-equal #t (boolean=? #t #t #t))
(check-equal #t (boolean=? #f #f #f))
(check-equal #t (boolean=? #t #t #t #t))
(check-equal #t (boolean=? #f #f #f #f))
(check-equal #f (boolean=? #t #f))
(check-equal #f (boolean=? #f #t))
(check-equal #f (boolean=? #t #f #f #f))
(check-equal #f (boolean=? #f #t #f #f))
(check-equal #f (boolean=? #f #f #t #f))
(check-equal #f (boolean=? #f #f #f #t))

;; char
(check-equal #t (char=? #\a #\a))
(check-equal #t (char=? #\a (string-ref "abc" 0)))
(check-equal #f (char=? #\b (string-ref "abc" 0)))
(check-equal #t (char=? #\a #\a #\a))
(check-equal #t (char=? #\a #\a #\a #\a))
(check-equal #f (char=? #\b #\a #\a #\a))
(check-equal #f (char=? #\a #\b #\a #\a))
(check-equal #f (char=? #\a #\a #\b #\a))
(check-equal #f (char=? #\a #\a #\a #\b))
(check-equal #t (char<? #\a #\b))
(check-equal #t (char<? #\a #\b #\c #\d))
(check-equal #f (char<? #\a #\a #\c #\d))
(check-equal #f (char<? #\a #\b #\a #\d))
(check-equal #f (char<? #\a #\b #\c #\a))
(check-equal #t (char<=? #\a #\b))
(check-equal #t (char<=? #\a #\b #\c #\d))
(check-equal #t (char>? #\b #\a))
(check-equal #t (char>=? #\b #\a))
(check-equal #t (char>=? #\b #\b))

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


;; Strings
(check-equal "" (string))
(check-equal "ab" (string #\a #\b))
(check-equal "abc" (string #\a #\b #\c))

(check-equal "abc" (substring "abc" 0 3))
(check-equal "" (substring "" 0 0))
(check-equal "abc" (substring "00abc00" 2 5))
(check-equal "" (substring "00abc00" 2 2))

(check-equal "" (string-append))
(check-equal "a" (string-append "a"))
(check-equal "ab" (string-append "a" (string #\b)))
(check-equal "abcabc" (string-append (string #\a #\b #\c) "abc"))

(check-equal '() (string->list ""))
(check-equal '(#\a) (string->list "a"))
(check-equal '(#\a #\b #\c) (string->list (string #\a #\b #\c)))
(check-equal '(#\a #\b) (string->list "000ab" 3))
(check-equal '(#\a #\b #\c) (string->list "000abc000" 3 6))
(check-equal '() (string->list "000" 2 2))

(check-equal "abc" (list->string '(#\a #\b #\c)))
(check-equal "" (list->string '()))

(check-equal "abc" (string-copy "abc"))
(check-equal "abc" (string-copy (string #\a #\b #\c)))
(check-equal "" (string-copy (string)))
(check-equal "" (string-copy ""))
(check-equal "abc00" (string-copy "00abc00" 2))

(check-equal "abc00" (copytest string-copy (string-copy! "00000" 0 "abc")))
(check-equal "0abc0" (copytest string-copy (string-copy! "00000" 1 "abc")))
(check-equal "00abc" (copytest string-copy (string-copy! "00000" 2 "abc")))
(check-equal "abc" (copytest string-copy (string-copy! "abc" 0 "")))
(check-equal "abc" (copytest string-copy (string-copy! "abc" 1 "")))
(check-equal "fg000" (copytest string-copy 
                               (string-copy! "00000" 0 "abcdefg" 5)))
(check-equal "ab000" (copytest string-copy (string-copy! "00000" 0 "abcdefg" 0 2)))

;; FIXME: Overwrap cases

(check-equal "" (copytest string-copy (string-fill! "" #\z)))
(check-equal "aaaaa" (copytest string-copy (string-fill! "01234" #\a)))
(check-equal "012aa" (copytest string-copy (string-fill! "01234" #\a 3)))
(check-equal "a1234" (copytest string-copy (string-fill! "01234" #\a 0 1)))
(check-equal "01234" (copytest string-copy (string-fill! "01234" #\z 0 0)))
(check-equal "01234" (copytest string-copy (string-fill! "01234" #\z 2 2)))

(check-equal "" (make-string 0))
(check-equal "a" (make-string 1 #\a))
(check-equal "aa" (make-string 2 #\a))
(check-equal "aaa" (make-string 3 #\a))

(check-equal #t (string=? "a" "a"))
(check-equal #t (string=? "a" "a" "a"))
(check-equal #t (string=? "aaa" "aaa"))
(check-equal #t (string=? "aaa" "aaa" "aaa"))
(check-equal #t (string=? "aaa" "aaa" "aaa" "aaa"))
(check-equal #t (string=? "aaa" "aaa" "aaa" "aaa" "aaa"))
(check-equal #t (string=? "" ""))
(check-equal #t (string=? "" "" ""))
(check-equal #t (string=? "" "" "" ""))
(check-equal #f (string=? "" "a"))
(check-equal #f (string=? "a" "a" "b"))
(check-equal #f (string=? "aaab" "aaaa"))
(check-equal #f (string=? "aa" "aa" "ab"))
(check-equal #f (string=? "aaaa" "aaa"))
(check-equal #f (string=? "aaaa" "aaaa" "aaaaa"))

(check-equal #f (string<? "a" "a"))
(check-equal #t (string<? "a" "b"))
(check-equal #t (string<? "aaaa" "aaab"))
(check-equal #t (string<? "a" "b" "c"))
(check-equal #t (string<? "aaaa" "aaab" "aaac"))
(check-equal #t (string<? "aaa" "aaaa"))
(check-equal #f (string<? "aaa" "aaa" "aaaa"))
(check-equal #f (string<? "aaa" "aab" "aab"))
(check-equal #f (string<? "aaab" "aaaa"))
(check-equal #f (string<? "aaab" "aaaa"))
(check-equal #f (string<? "aaaa" "aaa"))
(check-equal #f (string<? "aaaa" "aaaa" "aaa"))

(check-equal #t (string<=? "a" "a"))
(check-equal #t (string<=? "a" "b"))
(check-equal #t (string<=? "aaaa" "aaab"))
(check-equal #t (string<=? "a" "b" "c"))
(check-equal #t (string<=? "aaaa" "aaab" "aaac"))
(check-equal #t (string<=? "aaa" "aaaa"))
(check-equal #t (string<=? "aaa" "aaa" "aaaa"))
(check-equal #t (string<=? "aaa" "aab" "aab"))
(check-equal #f (string<=? "aaab" "aaaa"))
(check-equal #f (string<=? "aaaa" "aaa"))
(check-equal #f (string<=? "aaaa" "aaaa" "aaa"))

;; FIXME: string>?
(check-equal #f (string>? "a" "b"))
(check-equal #t (string>? "b" "a"))
;; FIXME: string>=?
(check-equal #f (string>=? "a" "b"))
(check-equal #t (string>=? "b" "a"))
(check-equal #t (string>=? "a" "a"))

;; Vectors

(check-equal '#() (vector))
(check-equal '#(1) (vector 1))
(check-equal '#(10 20) (vector 10 20))
(check-equal '#(10 20 30) (vector 10 20 30))
(check-equal '() (vector->list '#()))
(check-equal '() (vector->list (make-vector 0 #\a)))
(check-equal '(10) (vector->list (make-vector 1 10)))
(check-equal '(10 20 30) (vector->list '#(10 20 30)))
(check-equal "" (vector->string (make-vector 0)))
(check-equal "a" (vector->string '#(#\a)))
(check-equal "abc" (vector->string '#(#\a #\b #\c)))
(check-equal "abc" (vector->string '#(#\0 #\0 #\a #\b #\c) 2))
(check-equal "abc" (vector->string '#(#\0 #\0 #\a #\b #\c) 2 5))
(check-equal "" (vector->string '#(#\0 #\0 #\0) 1 1))
(check-equal '#() (string->vector ""))
(check-equal '#() (string->vector "abc" 1 1))
(check-equal '#(#\a) (string->vector "a"))
(check-equal '#(#\a #\b #\c) (string->vector "00abc" 2))
(check-equal '#(#\a #\b #\c) (string->vector "00abc00" 2 5))
(check-equal '#() (vector-copy (make-vector 0)))
(check-equal '#(10 20 30) (vector-copy (vector 10 20 30)))
(check-equal '#() (vector-copy (vector 10 20 30) 1 1))
(check-equal '#(20) (vector-copy (vector 10 20 30) 1 2))
(check-equal '#(20 30) (vector-copy (vector 10 20 30) 1))
(check-equal '#(20 30) (vector-copy (vector 10 20 30) 1 3))
(check-equal '#(10 20 30) (copytest vector-copy 
                                    (vector-copy! (make-vector 3)
                                                  0
                                                  (vector 10 20 30))))
(check-equal '#(10) (copytest vector-copy
                              (vector-copy! (make-vector 1)
                                            0
                                            (vector 10 20 30)
                                            0 1))) 
(check-equal '#(20 30) (copytest vector-copy
                                 (vector-copy! (make-vector 2)
                                               0
                                               (vector 10 20 30)
                                               1)))
(check-equal '#(10 20 30 40) (copytest vector-copy
                                       (vector-copy! (make-vector 4 10)
                                                    1
                                                    (vector 20 30 40))))

(check-equal '#(10 20 30) (vector-append (vector 10) (vector 20 30)))
(check-equal '#(10 20 30) (vector-append (vector 10 20 30)))
(check-equal '#(10 20 30) (vector-append (vector) (vector) (vector 10 20 30)
                                         (vector)))

(check-equal '#(10 10 10) (copytest vector-copy
                                    (vector-fill! (make-vector 3) 10)))
(check-equal '#(10 20 20) (copytest vector-copy
                                    (vector-fill! (make-vector 3 10) 20 1)))
(check-equal '#(10 20 10) (copytest vector-copy
                                    (vector-fill! (make-vector 3 10) 20 1 2)))
(check-equal '#() (copytest vector-copy
                            (vector-fill! (vector) 10)))

;; Bytevectors

(check-equal (make-bytevector 0) (bytevector))
(check-equal (make-bytevector 2 10) (bytevector 10 10))
(check-equal (make-bytevector 2 10) (bytevector-copy (bytevector 10 10)))
(check-equal (bytevector 20 20 20) (bytevector-copy (bytevector 10 10 20
                                                                20 20)
                                                    2))
(check-equal (bytevector 10 20) (bytevector-copy (bytevector 10 10 20 20 20
                                                             20)
                                                 1 3))
(check-equal (bytevector 10 20 30 40 50)
             (copytest bytevector-copy
                       (bytevector-copy! (bytevector 10 10 10 10 10)
                                         1
                                         (bytevector 20 30 40 50))))
(check-equal (bytevector 10 20 30 40 10)
             (copytest bytevector-copy
                       (bytevector-copy! (make-bytevector 5 10)
                                         1
                                         (bytevector 20 30 40 50)
                                         0 3)))
(check-equal (bytevector 10 20 30 40 50)
             (bytevector-append (bytevector)
                                (bytevector 10 20)
                                (bytevector)
                                (bytevector 30)
                                (bytevector 40 50))) 

(check-equal "abc" (utf8->string (bytevector 97 98 99)))
(check-equal (bytevector 97 98 99) (string->utf8 "abc"))
(check-equal "" (utf8->string (bytevector)))
(check-equal (bytevector) (string->utf8 ""))

;; map
(check-equal '() (map (lambda _ (error "should not be called")) '()))
(check-equal '() (map (lambda _ (error "should not be called")) '() '()))
(check-equal '() (map (lambda _ (error "should not be called")) '() '() '()))
(check-equal '() (map (lambda _ (error "should not be called")) '() '() '() '()))

(check-equal '(2 3 4 5) (map (lambda (e) (+ 1 e)) (list 1 2 3 4)))
(check-equal '(2 3 4 5) (map (lambda (e f) (check-equal e f)
                               (+ 1 e))
                             '(1 2 3 4)
                             '(1 2 3 4)))
(check-equal '(2 3 4 5) (map (lambda (e f g) 
                               (check-equal e f)
                               (check-equal e g)
                               (+ 1 e))
                             '(1 2 3 4)
                             '(1 2 3 4)
                             '(1 2 3 4)))
(check-equal '(2 3 4 5) (map (lambda (e f g h) 
                               (check-equal e f)
                               (check-equal e g)
                               (check-equal e h)
                               (+ 1 e))
                             '(1 2 3 4)
                             '(1 2 3 4)
                             '(1 2 3 4)
                             '(1 2 3 4)))

(check-equal 5 (begin (for-each (lambda _ (error "err.")) '()) 5))
(check-equal 5 (begin (for-each (lambda _ (error "err.")) '() '()) 5))
(check-equal 5 (begin (for-each (lambda _ (error "err.")) '() '() '()) 5))
(check-equal 5 (begin (for-each (lambda _ (error "err.")) '() '() '() '()) 5))

(check-equal 5 (let ((cnt 0))
                (for-each (lambda (e) 
                            (check-equal e 10)
                            (set! cnt (+ cnt 1)))
                          '(10 10 10 10 10))
                cnt))

(let ((s0 (string-copy "XXX")))
 (for-each (lambda (pos c) (string-set! s0 pos c))
           '(0 1 2)
           '(#\a #\i #\r))
 (check-equal "air" s0))

(let ((s0 (string-copy "XXXX"))
      (s1 (string-copy "YYYY")))
  (for-each (lambda (pos c d) 
              (string-set! s0 pos c) 
              (string-set! s1 pos d))
            '(0 1 2 3)
            '(#\m #\o #\o #\f)
            '(#\p #\o #\s #\e))
  (check-equal "moof" s0)
  (check-equal "pose" s1))

(let ((s0 (string-copy "XXXXX"))
      (s1 (string-copy "YYYYY"))
      (s2 (string-copy "ZZZZZ")))
  (for-each (lambda (pos c d e)
              (string-set! s0 pos c)
              (string-set! s1 pos d)
              (string-set! s2 pos e))
            '(0 1 2 3 4)
            '(#\s #\l #\a #\s #\h)
            '(#\q #\u #\i #\t #\e)
            '(#\1 #\2 #\3 #\4 #\5))
  (check-equal "slash" s0)
  (check-equal "quite" s1)
  (check-equal "12345" s2))

(check-equal "" (string-map (lambda _ (error "err.")) ""))
(check-equal "" (string-map (lambda _ (error "err.")) "" ""))
(check-equal "" (string-map (lambda _ (error "err.")) "" "" ""))
(check-equal "" (string-map (lambda _ (error "err.")) "" "" "" ""))

(check-equal 5 (begin (string-for-each (lambda _ (error "err."))
                                       "") 5))
(check-equal 5 (begin (string-for-each (lambda _ (error "err."))
                                       "" "") 5))
(check-equal 5 (begin (string-for-each (lambda _ (error "err."))
                                       "" "" "") 5))
(check-equal 5 (begin (string-for-each (lambda _ (error "err."))
                                       "" "" "" "") 5))
(define (next-char c) (integer->char (+ 1 (char->integer c))))
(define (the-digit c) (- (char->integer c)
                         (char->integer #\0)))


(check-equal "23456" (string-map (lambda (e) (next-char e)) "12345"))

(let ((s0 (string-copy "XXXXX")))
 (check-equal "12345"
              (string-map (lambda (e f) 
                            (string-set! s0 (the-digit e) f)
                            (next-char e))
                          "01234"
                          "slash"))
 (check-equal s0 "slash"))

(let ((s0 (string-copy "XXXX"))
      (s1 (string-copy "YYYY")))
  (check-equal "1234"
               (string-map (lambda (e f g)
                             (string-set! s0 (the-digit e) f)
                             (string-set! s1 (the-digit e) g)
                             (next-char e))
                           "0123"
                           "moof"
                           "pose"))
  (check-equal "moof" s0)
  (check-equal "pose" s1))

(let ((s0 (string-copy "XXX"))
      (s1 (string-copy "YYY"))
      (s2 (string-copy "ZZZ")))
  (check-equal "123"
               (string-map (lambda (e f g h)
                             (string-set! s0 (the-digit e) f)
                             (string-set! s1 (the-digit e) g)
                             (string-set! s2 (the-digit e) h)
                             (next-char e))
                           "012"
                           "air"
                           "dot"
                           "ram"))
  (check-equal "air" s0)
  (check-equal "dot" s1)
  (check-equal "ram" s2))


(let ((s0 (string-copy "XXXXX")))
 (string-for-each (lambda (e f) 
                    (string-set! s0 (the-digit e) f))
                  "01234"
                  "slash")
 (check-equal s0 "slash"))

(let ((s0 (string-copy "XXXX"))
      (s1 (string-copy "YYYY")))

  (string-for-each (lambda (e f g)
                     (string-set! s0 (the-digit e) f)
                     (string-set! s1 (the-digit e) g))
                   "0123"
                   "moof"
                   "pose")
  (check-equal "moof" s0)
  (check-equal "pose" s1))

(let ((s0 (string-copy "XXX"))
      (s1 (string-copy "YYY"))
      (s2 (string-copy "ZZZ")))

  (string-for-each (lambda (e f g h)
                     (string-set! s0 (the-digit e) f)
                     (string-set! s1 (the-digit e) g)
                     (string-set! s2 (the-digit e) h))
                   "012"
                   "air"
                   "dot"
                   "ram")
  (check-equal "air" s0)
  (check-equal "dot" s1)
  (check-equal "ram" s2))

(check-equal '#() (vector-map (lambda _ (error "err.")) '#()))
(check-equal '#() (vector-map (lambda _ (error "err.")) '#() '#()))
(check-equal '#() (vector-map (lambda _ (error "err.")) '#() '#() '#()))
(check-equal '#() (vector-map (lambda _ (error "err.")) '#() '#() '#() '#()))

(check-equal 5 (begin (vector-for-each (lambda _ (error "err."))
                                       '#()) 5))
(check-equal 5 (begin (vector-for-each (lambda _ (error "err."))
                                       '#() '#()) 5))
(check-equal 5 (begin (vector-for-each (lambda _ (error "err."))
                                       '#() '#() '#()) 5))
(check-equal 5 (begin (vector-for-each (lambda _ (error "err."))
                                       '#() '#() '#() '#()) 5))

(check-equal '#(1 2 3 4) (vector-map (lambda (e) (+ 1 e))
                                     (vector 0 1 2 3)))

(let ((v0 (make-vector 5)))
  (check-equal '#(1 2 3 4 5)
               (vector-map (lambda (e f) 
                             (vector-set! v0 e f)
                             (+ 1 e))
                           (vector 0 1 2 3 4)
                           (vector #\s #\l #\a #\s #\h)))
  (check-equal "slash" (vector->string v0)))

(let ((v0 (make-vector 4))
      (v1 (make-vector 4)))
  (check-equal '#(1 2 3 4)
               (vector-map (lambda (e f g)
                             (vector-set! v0 e f)
                             (vector-set! v1 e g)
                             (+ 1 e))
                           (vector 0 1 2 3)
                           (vector #\m #\o #\o #\f)
                           (vector #\p #\o #\s #\e)))
  (check-equal "moof" (vector->string v0))
  (check-equal "pose" (vector->string v1)))

(let ((v0 (make-vector 3))
      (v1 (make-vector 3))
      (v2 (make-vector 3)))
  (check-equal '#(1 2 3)
               (vector-map (lambda (e f g h)
                             (vector-set! v0 e f)
                             (vector-set! v1 e g)
                             (vector-set! v2 e h)
                             (+ 1 e))
                           (vector 0 1 2)
                           (vector #\a #\i #\r)
                           (vector #\d #\o #\t)
                           (vector #\r #\a #\m)) )
  (check-equal "air" (vector->string v0))
  (check-equal "dot" (vector->string v1))
  (check-equal "ram" (vector->string v2)))

(let ((v0 (make-vector 5)))

 (vector-for-each (lambda (e f) 
               (vector-set! v0 e f))
                  (vector 0 1 2 3 4)
                  (vector #\s #\l #\a #\s #\h))
 (check-equal "slash" (vector->string v0)))

(let ((v0 (make-vector 4))
      (v1 (make-vector 4)))

  (vector-for-each (lambda (e f g)
                     (vector-set! v0 e f)
                     (vector-set! v1 e g))
                   (vector 0 1 2 3)
                   (vector #\m #\o #\o #\f)
                   (vector #\p #\o #\s #\e))
  (check-equal "moof" (vector->string v0))
  (check-equal "pose" (vector->string v1)))

(let ((v0 (make-vector 3))
      (v1 (make-vector 3))
      (v2 (make-vector 3)))

  (vector-for-each (lambda (e f g h)
                     (vector-set! v0 e f)
                     (vector-set! v1 e g)
                     (vector-set! v2 e h))
                   (vector 0 1 2)
                   (vector #\a #\i #\r)
                   (vector #\d #\o #\t)
                   (vector #\r #\a #\m)) 
  (check-equal "air" (vector->string v0))
  (check-equal "dot" (vector->string v1))
  (check-equal "ram" (vector->string v2)))

;; assoc
(check-equal '(2 4) (assoc 2.0 '((1 1) (2 4) (3 9)) =)) ;; From R7RS

;; member
(define (pick-b a b)
  (or (and (string? a) (string=? a "b"))
      (and (string? b) (string=? b "b"))))

(check-equal '("b" "c") (member 'any '("a" "b" "c") pick-b))

(check-finish)
