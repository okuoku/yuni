(define (yuni/gensym sym) (gensym sym))
(define (yuni/identifier? x) (symbol? x))
(define *yuni/libalias*
  '((yuni . gambit-yuni)
    (scheme . gambit-compat-scheme)))


