(library (yuni util invalid-form)
         (export define-invalid-form define-invalid-forms)
         (import (scheme base))

(define-syntax define-invalid-form
  (syntax-rules () 
    ((_ sym)
     (define-syntax sym
       (syntax-rules () 
         ((_)
          (syntax-error "Invalid form (for aux keyword)")))))))

(define-syntax define-invalid-forms
  (syntax-rules ()
    ((_ sym)
     (define-invalid-form sym))
    ((_ sym0 sym1 ...)
     (begin
       (define-invalid-form sym0)
       (define-invalid-forms sym1 ...)))))

)
