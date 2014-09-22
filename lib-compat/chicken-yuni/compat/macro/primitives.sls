(library (chicken-yuni compat macro primitives)
         (export define-inject-syntax)
         (import (chicken) (matchable))

(define-syntax define-inject-syntax
  (syntax-rules ()
    ((_ nam syms orig-k)
     (expand-syms syms orig-k nam))))

(define-syntax expand-syms
  (er-macro-transformer
    (lambda (stx here _0)
      (define (conv lis)
        (map (lambda (s)
               (cond ((symbol? s) (symbol->string s))
                     ((string? s) s)
                     (else
                       (error "Invalid object as part of identifier" s))))
             lis))
      (match stx
             ((_ (x ...) orig-k nam)
              (let ((sym (string->symbol (apply string-append (conv x))))
                    (k (here 'syntax-inject-entry)))
               `(,k ,sym ,orig-k ,nam)))))))

(define-syntax syntax-inject-entry
  (syntax-rules ()
    ((_ sym k name)
     (begin
       (define-syntax b
         (er-macro-transformer 
           (lambda (y here _2)
             (match y
                    ((_ obj) obj)
                    ((_ bind prog)
                     `(,(here 'letrec)
                        ((sym ,bind))
                        ,prog)))))) 
       (define-syntax name
         (syntax-rules ()
           ((_ param)
            (k b sym param))))))))
)
