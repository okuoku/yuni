(define-module yuni-runtime.gauche.macro-primitives
               (use gauche)
               (export define-inject-syntax))

(select-module yuni-runtime.gauche.macro-primitives)

(define (conv lis)
        (map (lambda (s)
               (cond ((symbol? s) (symbol->string s))
                     ((string? s) s)
                     (else
                       (error "Invalid object as part of identifier" s))))
             lis))

(define-macro (expand-syms k b syms param)
  (define sym-value (conv syms))
  (list k b sym-value param))

(define-syntax define-inject-syntax
  (syntax-rules ()
    ((_ nam syms k)
     (begin
       (define sym-value (conv 'syms))
       (define-macro b
         (case-lambda
           ((form)
            form)
           ((bind prog)
            `(letrec ((,sym-value ,bind)) ,prog))))
       (define-syntax nam
         (syntax-rules ()
           ((_ param)
            (expand-syms k b syms param))))))))

