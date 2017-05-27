(library (yunivmrt scheme-syntax)
         (export
           ;; (r7c-basic syntax define)
           define
           ;; (r7c-basic syntax quasiquote)
           quasiquote unquote unquote-splicing
           ;; (r7c-report binding-construct letrec)
           letrec letrec*
           ;; (r7c-report binding-construct let)
           let let*
           ;; (r7c-report binding-construct let-values)
           let-values let*-values
           ;; (r7c-report conditional and)
           and
           ;; (r7c-report conditional or)
           or
           ;; (r7c-report conditional when)
           when
           ;; (r7c-report conditional unless)
           unless
           ;; (r7c-report conditional cond)
           cond
           ;; (r7c-report conditional case)
           case
           ;; (r7c-report misc case-lambda)
           case-lambda
           ;; (r7c-report misc do)
           do
           ;; (r7c-report misc guard)
           guard
           ;; (r7c-report binding-construct define-values)
           define-values

           ;; (yunivmrt coresyntax)
           _ ... => else syntax-rules
           let-syntax letrec-syntax
           define-syntax
           ;; FIXME:
           parameterize
           syntax-error
           define-record-type 

           ;; yunifake specific
           begin
           if 
           lambda
           quote
           set!)
         (import 
           (yunivmrt coresyntax)
           (r7c-basic syntax define)
           (r7c-basic syntax quasiquote)
           (r7c-report binding-construct letrec)
           (r7c-report binding-construct let)
           (r7c-report binding-construct let-values)
           (r7c-report binding-construct define-values)
           (r7c-report conditional and)
           (r7c-report conditional or)
           (r7c-report conditional when)
           (r7c-report conditional unless)
           (r7c-report conditional cond)
           (r7c-report conditional case)
           (r7c-report misc case-lambda)
           (r7c-report misc do)
           (r7c-report misc guard))
)
