(library (yuni ffi database exports)
         (export
           make-exports
           exports-add-entry!
           exports-entries
           
           make-export
           export-name
           export-type
           export-macro?
           export-macro?-set!
           )
         (import (yuni scheme)
                 (yuni core))

;; export

(define* exports (exportentries*))

(define* export 
  (name
    type
    macro?))

(define (make-exports)
  (make exports
        (exportentries* '())))

(define (make-export type name)
  (make export
        (name name)
        (type type)
        (macro? #f)))

(define* (exports-entries (exports))
  (~ exports 'exportentries*))

(define* (exports-add-entry! (exports) (export))
  (define s (exports-entries exports))
  (~ exports 'exportentries* := (cons export s)))

(define* (export-name (export))
  (~ export 'name))

(define* (export-type (export))
  (~ export 'type))

(define* (export-macro? (export))
  (~ export 'macro?))

(define* (export-macro?-set! (export) b)
  (~ export 'macro? := b))

)
