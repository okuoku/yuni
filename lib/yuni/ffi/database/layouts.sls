(library (yuni ffi database layouts)
         (export 
           make-layouts
           layouts-aggregates
           layouts-add-aggregate!

           make-aggregate
           aggregate-name
           aggregate-entries
           aggregate-add-entry!

           make-aggregate-entry
           aggregate-entry-name
           aggregate-entry-array?-set!
           aggregate-entry-array?
           aggregate-entry-add-constraint!
           aggregate-entry-constraints
           )
         (import (yuni scheme)
                 (yuni base match)
                 (yuni core))
         
;; layouts

(define* layouts (aggregates*))

(define* aggregate (name entries*)) ;; FIXME: add constraint

(define* aggregate-entry
  (name
    type
    array?
    constraints*))

(define (make-aggregate name)
  (make aggregate
        (name name)
        (entries* '())))

(define* (make-layouts)
  (make layouts
        (aggregates* '())))

(define* (layouts-aggregates (layouts))
  (~ layouts 'aggregates*)) 

(define* (layouts-add-aggregate! (layouts) (aggregate))
  (define s (~ layouts 'aggregates*))
  (~ layouts 'aggregates* := (cons aggregate s)))


(define* (aggregate-name (aggregate))
  (~ aggregate 'name))

(define* (aggregate-entries (aggregate))
  (~ aggregate 'entries*))

(define* (aggregate-add-entry! (aggregate) (aggregate-entry))
  (define s (aggregate-entries aggregate))
  (~ aggregate 'entries* := (cons aggregate-entry s)))


(define (make-aggregate-entry type name)
  (make aggregate-entry
        (type type)
        (name name)
        (array? #f)
        (constraints* '())))

(define* (aggregate-entry-name (aggregate-entry))
  (~ aggregate-entry 'name))

(define* (aggregate-entry-array?-set! (aggregate-entry) b)
  (~ aggregate-entry 'array? := b))

(define* (aggregate-entry-array? (aggregate-entry))
  (~ aggregate-entry 'array?))

(define* (aggregate-entry-constraints (aggregate-entry))
  (~ aggregate-entry 'constraints*))

(define* (aggregate-entry-add-constraint! (aggregate-entry) c)
  (define s (aggregate-entry-constraints aggregate-entry))
  (~ aggregate-entry 'constraints* := (cons c s)))

)
