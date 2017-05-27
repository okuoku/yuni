(library (yunivmrt coresyntax)
         (export
           _ ... =>
           unquote unquote-splicing
           syntax-rules
           else
           let-syntax letrec-syntax
           define-syntax
           ;; FIXME
           parameterize
           syntax-error
           define-record-type
           define

           ;; defined here.
           begin
           if
           lambda
           quote
           set!
           $define/core
           $let/core)
         (import (yunivmrt expander-callbacks)
                 (yunifake bogus))

(define-syntax-names/yunifake
  _ ... =>
  unquote unquote-splicing
  syntax-rules
  let-syntax letrec-syntax
  define-syntax
  else

  ;; Expander core syntax
  begin
  if
  lambda
  quote
  set!
  
  ;; extra
  define
  ;; FIXME
  parameterize
  syntax-error
  define-record-type
  )

(define-syntax $$let/remap
  (syntax-rules ()
    ((_ (vars ()) () . cmd)
     ($$yunifake-inject-primitive/raw let vars . cmd))
    ((_ (vars (init0 . init*))  (var0 . var*) . cmd)
     ($$let/remap (((var0 init0) . vars) init*) var* . cmd))))

(define-syntax $let/core
  (syntax-rules ()
    ((_ ((var init) ...) . body)
     ($$yunifake-bind/body
       $$let/remap
       (() (init ...))
       (var ...)
       . body))))


(define-syntax $define/core
  (syntax-rules ()
    ((_ nam body)
     (define nam body))))
         
)
