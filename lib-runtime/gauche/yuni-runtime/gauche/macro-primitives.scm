(define-module yuni-runtime.gauche.macro-primitives
               ;(use gauche)
               (export define-inject-syntax))

(select-module yuni-runtime.gauche.macro-primitives)

(define (conv lis)
        (map (lambda (s)
               (cond ((symbol? s) (symbol->string s))
                     ((string? s) s)
                     (else
                       (error "Invalid object as part of identifier" s))))
             lis))

(define-macro (define-inject-syntax name syms k)
  (let ((sym-value-value (string->symbol (apply string-append (conv syms))))
        (expand-syms (gensym))
        (sym-value (gensym))
        (b (gensym)))
    `(begin
       (define ,sym-value ',sym-value-value)
       (define-macro ,b
         (lambda x
           (case (length x)
             ((1) (car x))
             ((2) `(letrec ((,,sym-value ,(car x))) ,(cadr x)))
             (else (error "INVALID K ON DEFINE-INJECT-SYNTAX")))))
       (define-macro (,name param)
         (list ,k ,b ,sym-value param)))))

