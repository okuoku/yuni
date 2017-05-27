(library (yunivmrt expander-callbacks)
         (export
           $$yunifake-inject-primitive/raw
           $$yunifake-inject-primitive
           $$yunifake-inject
           $$yunifake-bind
           $$yunifake-bind/body
           $$yunifake-expand-expr
           $$yunifake-expand-body
           $$yunifake-inject-definition)
         (import)

;;

(define-syntax $$yunifake-inject-primitive/raw
  (syntax-rules ()
    ((_ prim . body)
     ($$yunifake-callback 0 prim . body))))

(define-syntax $$yunifake-inject-primitive
  (syntax-rules ()
    ((_ prim . body)
     ($$yunifake-callback 1 prim . body))))

(define-syntax $$yunifake-inject
  (syntax-rules ()
    ((_ sym body ...)
     ($$yunifake-callback 2 sym body ...))))

(define-syntax $$yunifake-expand-expr
  (syntax-rules ()
    ((_ sexp)
     ($$yunifake-callback 3 sexp))))

(define-syntax $$yunifake-expand-body
  (syntax-rules ()
    ((_ body ...)
     ($$yunifake-callback 4 body ...))))

(define-syntax $$yunifake-bind
  (syntax-rules ()
    ((_ cb cb-arg (frm ...) body ...)
     ($$yunifake-callback 5 cb cb-arg (frm ...) body ...))))

(define-syntax $$yunifake-bind/body
  (syntax-rules ()
    ((_ cb cb-arg (frm ...) body ...)
     ($$yunifake-callback 6 cb cb-arg (frm ...) body ...))))

(define-syntax $$yunifake-inject-definition
  (syntax-rules ()
    ((_ nam frm)
     ($$yunifake-callback 7 nam frm))))

         
)
