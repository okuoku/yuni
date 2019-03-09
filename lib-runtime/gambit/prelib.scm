(define (yuni/gensym sym) (gensym sym))
(define (yuni/identifier? x) (symbol? x))
(define *yuni/libalias*
  '((yuni . gambit-yuni)
    (scheme . gambit-compat-scheme)))

(define-macro (let-values cls* . body)
  (if (pair? cls*)
    (let ((c (car cls*))
          (d (cdr cls*)))
      `(receive ,(car c) ,(cadr c)
         (let-values ,d ,@body)))
    `(let () ,@body)))

;; FIXME: yunifake leftover
(define-macro (define-primitive-names/yunifake . bogus)
  `(begin))
