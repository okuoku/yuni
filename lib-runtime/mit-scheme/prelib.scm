(define (yuni/gensym sym) (gensym sym))
(define (yuni/identifier? x) (symbol? x))
(define *yuni/libalias*
  '((yuni . mit-scheme-yuni)
    (scheme . mit-scheme-compat-scheme)))

(define-syntax define-macro
  (er-macro-transformer
    (lambda (x r c)
      (let ((name (caadr x))
            (args (cdadr x))
            (body (cddr x))
            (define-syntax (r 'define-syntax))
            (er-macro-transformer (r 'er-macro-transformer))
            (lambda (r 'lambda))
            (x (r 'x))
            (r (r 'r))
            (c (r 'c)))
        `(,define-syntax 
           name
           (,er-macro-transformer
             (,lambda (,x ,r ,c)
                      ((,lambda ,args ,body)
                       ,x))))))))

