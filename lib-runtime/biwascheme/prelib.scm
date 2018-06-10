(define (yuni/gensym bogus) (gensym))
(define (yuni/identifier? x) (symbol? x))
(define yuni/args (cdddr (command-line))) ;; Drop /usr/bin/node, biwas itself
(define (yuni/command-line) yuni/args)
(define (yuni/update-command-line! lis) (set! yuni/args lis))
(define (exact x) (truncate x))
(define (inexact x) x)
(define (inexact? x) #t)
(define (exact? x) (= x (truncate x)))
