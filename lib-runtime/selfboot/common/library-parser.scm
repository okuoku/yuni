(define (%selfboot-library-name sexp)
  (cadr sexp))

(define (%selfboot-library-depends sexp)
  (cdr (cadddr sexp)))

(define (%selfboot-program-depends sexp)
  (cdr (car sexp)))

