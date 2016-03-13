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

(define (interfacelib) (cons lib (libs)))
         
)
