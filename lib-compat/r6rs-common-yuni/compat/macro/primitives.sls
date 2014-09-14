;; Bridge library to export identifiers on multiple meta-levels
(library (r6rs-common-yuni compat macro primitives)
         (export define-inject-syntax)
         (import 
           (rnrs)
           (for (r6rs-common-yuni compat macro primitives0) run expand))
         
(define-syntax define-inject-syntax
  (syntax-rules ()
    ((_ name sym k)
     (define-syntax name (syntax-inject sym k)))))
         
)
