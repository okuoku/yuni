(import (yuni scheme)
        (yunitest mini))

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

(let ((s0 (string->vector "XXXXX")))
 (check-equal "12345"
              (string-map (lambda (e f)
                            (vector-set! s0 (the-digit e) f)
                            (next-char e))
                          "01234"
                          "slash"))
 (check-equal (vector->string s0) "slash"))

(let ((s0 (string->vector "XXXX"))
      (s1 (string->vector "YYYY")))
  (check-equal "1234"
               (string-map (lambda (e f g)
                             (vector-set! s0 (the-digit e) f)
                             (vector-set! s1 (the-digit e) g)
                             (next-char e))
                           "0123"
                           "moof"
                           "pose"))
  (check-equal "moof" (vector->string s0))
  (check-equal "pose" (vector->string s1)))

(let ((s0 (string->vector "XXX"))
      (s1 (string->vector "YYY"))
      (s2 (string->vector "ZZZ")))
  (check-equal "123"
               (string-map (lambda (e f g h)
                             (vector-set! s0 (the-digit e) f)
                             (vector-set! s1 (the-digit e) g)
                             (vector-set! s2 (the-digit e) h)
                             (next-char e))
                           "012"
                           "air"
                           "dot"
                           "ram"))
  (check-equal "air" (vector->string s0))
  (check-equal "dot" (vector->string s1))
  (check-equal "ram" (vector->string s2)))

(let ((s0 (string->vector "XXXXX")))
 (string-for-each (lambda (e f)
                    (vector-set! s0 (the-digit e) f))
                  "01234"
                  "slash")
 (check-equal (vector->string s0) "slash"))

(let ((s0 (string->vector "XXXX"))
      (s1 (string->vector "YYYY")))

  (string-for-each (lambda (e f g)
                     (vector-set! s0 (the-digit e) f)
                     (vector-set! s1 (the-digit e) g))
                   "0123"
                   "moof"
                   "pose")
  (check-equal "moof" (vector->string s0))
  (check-equal "pose" (vector->string s1)))

(let ((s0 (string->vector "XXX"))
      (s1 (string->vector "YYY"))
      (s2 (string->vector "ZZZ")))

  (string-for-each (lambda (e f g h)
                     (vector-set! s0 (the-digit e) f)
                     (vector-set! s1 (the-digit e) g)
                     (vector-set! s2 (the-digit e) h))
                   "012"
                   "air"
                   "dot"
                   "ram")
  (check-equal "air" (vector->string s0))
  (check-equal "dot" (vector->string s1))
  (check-equal "ram" (vector->string s2)))


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
