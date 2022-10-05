(library (yunife-yunivm runtime unsupported)
         (export
           let-syntax
           letrec-syntax
           define-record-type
           define-values
           guard
           parameterize)
         (import (yunivm-core-syntax))

         
(define-syntax let-syntax
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
(define-syntax letrec-syntax
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
(define-syntax define-record-type
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
(define-syntax define-values
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
(define-syntax guard
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
(define-syntax parameterize
  (syntax-rules ()
    ((nam . bogus)
     (syntax-error "Not supported" nam))))
)
