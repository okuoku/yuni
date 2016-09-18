(library (yunifake-util expander-callbacks)
         (export
           $$yunifake-inject-primitive/raw
           $$yunifake-inject-primitive
           $$yunifake-inject
           $$yunifake-bind
           $$yunifake-expand-expr
           $$yunifake-expand-body)
         (import)

;;

(define-syntax $$yunifake-inject-primitive/raw
  (syntax-rules ()
    ((_ prim body ...)
     ($$yunifake-callback 0 prim body ...))))

(define-syntax $$yunifake-inject-primitive
  (syntax-rules ()
    ((_ prim body ...)
     ($$yunifake-callback 1 prim body ...))))

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
    ((_ (frm ...) body ...)
     ($$yunifake-callback 5 (frm ...) body ...))))

         
)
