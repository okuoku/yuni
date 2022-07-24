(import (yuni scheme)
        (yuni compat ident)
        (yunitest mini))


(define (gen-negative-zero)
  (define (loop v n)
    (if (= n 60000)
      (error "Unexpected loop count" n)
      (or (and (= 0.0 v) v)
          (loop (/ v 2.0) (+ n 1)))))
  (loop (- 1.0) 0))

(define (gen-negative-zero2) (* -1.0 0.0))

(define old-float-format (case (ident-impl)
                           ((gambit mit-scheme scm) #t)
                           (else #f)))
(define source-reader-negative-zero -0.0)
(define number-reader-negative-zero (string->number "-0.0"))
(define generated-negative-zero (gen-negative-zero))
(define generated-negative-zero2 (gen-negative-zero2))

(define input* (list source-reader-negative-zero
                     number-reader-negative-zero
                     generated-negative-zero
                     generated-negative-zero2))

(define map-eqvzero (map (lambda (e) (eqv? e 0.0)) input*))
(define map-equalzero (map (lambda (e) (equal? e 0.0)) input*))
(define map-=zero (map (lambda (e) (= e 0.0)) input*))
(define map-strcompzero (map (lambda (e) 
                               (char=? #\- (string-ref (number->string e) 0)))
                             input*))

(define have-rnrs-zero-compare (equal? '(#f #f #f #f) map-eqvzero))
(define printable-negative-zero (case (ident-impl)
                                  ((digamma cyclone bigloo s7 stklos)
                                   (not have-rnrs-zero-compare))
                                  (else have-rnrs-zero-compare)))
(define expected-string
  (string-append (if printable-negative-zero "-" "")
                 (if old-float-format "0." "0.0")))

(check-equal (case (ident-impl)
               ((digamma cyclone ironscheme bigloo s7 scm stklos)
                '(#t #t #t #t))
               (else '(#f #f #f #f))) 
             map-eqvzero)

(check-equal expected-string (number->string source-reader-negative-zero))
(check-equal expected-string (number->string number-reader-negative-zero))
(check-equal expected-string (number->string generated-negative-zero))

(cond
  (have-rnrs-zero-compare
    (check-equal #f (eqv? -0.0 0.0))
    (check-equal #f (eqv? 0.0 -0.0)))
  (else
    (check-equal #t (eqv? -0.0 0.0))
    (check-equal #t (eqv? 0.0 -0.0))))

(check-equal #t (eqv? 0.0 0.0))
(check-equal #t (eqv? -0.0 -0.0))
(check-equal #f (negative? 0.0))
(check-equal #f (positive? 0.0))
(check-equal #f (negative? -0.0))
(check-equal #f (positive? -0.0)) 

(write (number->string (gen-negative-zero2))) (newline)
(check-equal map-eqvzero map-equalzero)
(check-equal '(#t #t #t #t) map-=zero)
(check-equal (case (ident-impl)
               ((scm ironscheme) 
                ;; SCM: print feature error
                '(#f #f #f #f))
               (else '(#t #t #t #t))) 
             map-strcompzero)

(check-finish)
