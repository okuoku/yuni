(library (yuni minife interfacelib)
         (export interfacelib
                 ;; Internal
                 id-$define/primitive?
                 id-$define-aux-syntax?
                 id-$bind-variable?
                 id-$bind-definition?
                 id-$extend-env?
                 id-$inject?
                 id-$inject/multi?
                 id-$inject/form?
                 id-$inject/splice?
                 id-$alias?
                 id-$quote?
                 ;; Plain
                 id-define-syntax?
                 id-syntax-error?
                 id-syntax-rules?)
         (import (yuni scheme)
                 (yuni minife identifiers)
                 (yuni minife environments))

(define lib '(r7c-expander-interface))

(define-syntax xid
  (syntax-rules ()
    ((_ sym)
     (id-new-library-macro 'sym 'sym 
                           ;; FIXME: should match with lib above
                           '(r7c-expander-interface)))))

(define %$define/primitive  (xid $define/primitive)  )
(define %$define-aux-syntax (xid $define-aux-syntax) )
(define %$bind-variable     (xid $bind-variable)     )
(define %$bind-definition   (xid $bind-definition)   )
(define %$extend-env        (xid $extend-env)        )
(define %$inject            (xid $inject)            )
(define %$inject/splice     (xid $inject/splice)     )
(define %$inject/multi      (xid $inject/multi)      )
(define %$inject/form       (xid $inject/form)       )
(define %$alias             (xid $alias)             )
(define %$quote             (xid $quote)             )
(define %define-syntax      (xid define-syntax)      )
(define %syntax-rules       (xid syntax-rules)       )
(define %syntax-error       (xid syntax-error)       )

(define (id-$define/primitive? x) (eq? x %$define/primitive))
(define (id-$define-aux-syntax? x) (eq? x %$define-aux-syntax))
(define (id-$bind-variable? x) (eq? x %$bind-variable))
(define (id-$bind-definition? x) (eq? x %$bind-definition))
(define (id-$extend-env? x) (eq? x %$extend-env))
(define (id-$inject? x) (eq? x %$inject))
(define (id-$inject/splice? x) (eq? x %$inject/splice))
(define (id-$inject/multi? x) (eq? x %$inject/multi))
(define (id-$inject/form? x) (eq? x %$inject/form))
(define (id-$alias? x) (eq? x %$alias))
(define (id-$quote? x) (eq? x %$quote))
(define (id-define-syntax? x) (eq? x %define-syntax))
(define (id-syntax-rules? x) (eq? x %syntax-rules))
(define (id-syntax-error? x) (eq? x %syntax-error))

(define (libs)
  (let ((ef (envframe-new)))
   (for-each (lambda (s e)
               (envframe-add-binding! ef s e)
               )
             '($define/primitive
               $define-aux-syntax
               $bind-variable
               $bind-definition
               $extend-env
               $inject
               $inject/splice
               $inject/multi
               $inject/form
               $alias
               $quote
               define-syntax
               syntax-rules
               syntax-error
               )
             (list
               %$define/primitive
               %$define-aux-syntax
               %$bind-variable
               %$bind-definition
               %$extend-env
               %$inject
               %$inject/splice
               %$inject/multi
               %$inject/form
               %$alias
               %$quote
               %define-syntax
               %syntax-rules
               %syntax-error
               ))
   ef))

#| Not compatible with R5RS??

(define-syntax defif
  (syntax-rules ()
    ((_ total (symid symname predname) ...)
     (begin
       (begin 
         (define symid (id-new-library-macro 'symname 'symname lib))
         (define (predname id) (eq? id symid)))
       ...
       (define (total)
         (let ((ef (envframe-new)))
          (envframe-add-binding! ef 'symname symid)
          ...
          ef))))))


(defif libs
  (%$define/primitive  $define/primitive  id-$define/primitive?)
  (%$define-aux-syntax $define-aux-syntax id-$define-aux-syntax?)
  (%$bind-variable     $bind-variable     id-$bind-variable?)
  (%$bind-definition   $bind-definition   id-$bind-definition?)
  (%$extend-env        $extend-env        id-$extend-env?)
  (%$inject            $inject            id-$inject?)
  (%$inject/splice     $inject/splice     id-$inject/splice?)
  (%$inject/multi      $inject/multi      id-$inject/multi?)
  (%$inject/form       $inject/form       id-$inject/form?)
  (%$alias             $alias             id-$alias?)
  (%$quote             $quote             id-$quote?)
  (%define-syntax      define-syntax      id-define-syntax?)
  (%syntax-rules       syntax-rules       id-syntax-rules?)
  (%syntax-error       syntax-error       id-syntax-error?))

|#

(define (interfacelib) (cons lib (libs)))
         
)
