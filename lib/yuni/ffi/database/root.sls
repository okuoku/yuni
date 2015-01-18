(library (yuni ffi database root)
         (export
           make-database
           database-libinfo
           database-libinfo-set!
           database-config
           database-config-set!
           database-prologue
           database-prologue-set!
           database-types
           database-types-set!
           database-layouts
           database-layouts-set!
           database-functions
           database-functions-set!
           database-exports
           database-exports-set!)
         (import (yuni scheme)
                 (yuni core))
         
;; root object
(define* root
  (libinfo config prologue types layouts functions exports))

(define-syntax def
  (syntax-rules ()
    ((_ sym getter setter)
     (begin 
       (define* (getter (root))
         (~ root 'sym))
       (define* (setter (root) x)
         (~ root 'sym := x))))))


(def libinfo database-libinfo database-libinfo-set!)
(def config database-config database-config-set!)
(def prologue database-prologue database-prologue-set!)
(def types database-types database-types-set!)
(def layouts database-layouts database-layouts-set!)
(def functions database-functions database-functions-set!)
(def exports database-exports database-exports-set!)

(define (make-database)
  (make root
        (libinfo #f)
        (config #f)
        (prologue #f)
        (types #f)
        (layouts #f)
        (functions #f)
        (exports #f)))

)
