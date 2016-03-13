(library (yuni minife expander)
         (export expand-forms!)
         (import (yuni scheme)
                 (yuni minife identifiers)
                 (yuni minife environments)
                 (yuni minife interfacelib))

;;
(define (expand! top-level? forms env cb-gensym)
  ;; (cb-gensym pair symname global?) => global-sym
  (define (re-enter) (expand! top-level? forms env cb-gensym))
  (define (next) (expand! top-level? (cdr forms) env cb-gensym))
  (define (enter p) (expand! top-level? p env cb-gensym))
  (define (gensym p orig) (cb-gensym p orig top-level?))
  (define (erase-next)
    (let ((d (cdr forms)))
     (cond
       ((pair? d)
        (set-car! forms (car d))
        (set-cdr! forms (cdr d))
        (re-enter))
       ((null? d)
        ;; FIXME: ...
        (set-car! forms '(begin)))
       (else
         (error "oops" forms)))))

  (define (expand-car! maybe-macro? thepair env)
    (define (handle-variable lookup)
      (set-car! thepair (id-global-name lookup)))
    (define (handle-unknown)
      (env-current-add-unknown! env thepair))
    (unless (pair? thepair)
      (error "pair required" thepair))
    (let ((a (car thepair)))
     (cond
       ((symbol? a)
        (let ((lookup (env-lookup env a)))
         (cond
           (maybe-macro?
             (cond
               ((and lookup (id-macro? lookup))
                (set-car! thepair lookup))
               (lookup (handle-variable lookup))
               (else   (handle-unknown))))
           (lookup
             (when (id-macro? lookup)
               (error "macro not allowed here" thepair))
             (handle-variable lookup))
           (else (handle-unknown))))))))

  (define (expand-variable! top-level? forms env)
    (unless (or (pair? forms) (null? forms))
      (error "huh?" forms))
    (when (pair? forms)
      (let ((a (car forms)))
       (cond 
         ((pair? a)
          (expand! top-level? forms env cb-gensym))
         (else
           (expand-car! #f forms env)
           (expand-variable! top-level? (cdr forms) env))))))

  (define (expand-$inject splice-env? multi-env? forms a)
    ;; ($inject        sym body ...)
    ;; ($inject/splice sym body ...)
    ;; ($inject/multi  sym body ...)
    (define new-frame? (not (or splice-env? multi-env?)))
    (let ((sym (cadr a))
          (body* (cddr a)))
      (when new-frame?
        (env-new-envframe! env))
      (expand! (and splice-env? top-level?) body* env cb-gensym)
      (when new-frame?
        (env-up/merge! env))
      (set-car! forms (cdr a))
      (next)))

  (define (expand-$inject/form forms a)
    ;; ($inject/form body ...)
    (let ((body* (cdr a)))
     (expand! top-level? body* env cb-gensym)
     (set-car! forms body*)))

  (cond
    ((pair? forms)
     (let ((a (car forms)))
      (cond
        ((pair? a)
         (let ((i (car a)))
          ;; Invoke
          (cond
            ((symbol? i)
             (expand-car! #t a env)
             (let ((i2 (car a)))
              (cond
                ((symbol? i2)
                 ;; expand arguments
                 (expand-variable! top-level? (cdr a) env)
                 (next))
                (else (re-enter)))))
            ((pair? i)
             (enter a)
             (next))
            ((not (id? i))
             (error "Invalid invocation" a))
            ((id-$bind-variable? i)
             ;; ($bind-variable sym)
             (let ((sym (cadr a)))
              (unless (symbol? sym)
                (error "symbol required" a))
              (envframe-decl-binding! (env-current-frame env)
                                      sym
                                      (id-new-variable sym))
              (set-car! forms sym)
              (next)))
            ((id-$bind-definition? i)
             ;; ($bind-definition sym)
             (let ((sym (cadr a)))
              (unless (symbol? sym)
                (error "symbol required" a))
              (let ((id (id-new-variable sym))
                    (gsym (gensym a sym)))
               (id-set-global-name! id gsym)
               (envframe-def-binding! (env-current-frame env) sym id)
               (set-car! forms gsym)
               (next))))
            ((id-$extend-env? i)
             ;; ($extend-env (sym ...) body ...)
             (let ((sym* (cadr a))
                   (body* (cddr a)))
               ; Extend env with (sym ...)
               (let ((cf (env-current-frame env)))
                (for-each
                  (lambda (sym) 
                    ;; FIXME: Do we really need them? Just re-add
                    ;;        IDs regardless of its decl/def types?
                    ;;        (No, we'd need this when we support
                    ;;         precise location recording)
                    (or (let ((decl (envframe-lookup-decl cf sym)))
                         (and decl
                              (begin
                                (envframe-add-binding! cf sym decl)
                                #t)))
                        (let ((def (envframe-lookup-def cf sym)))
                         (and def
                              (begin
                                (envframe-add-binding! cf sym def)
                                #t)))
                        (begin
                          (envframe-add-binding! cf sym
                                                 (id-new-variable sym)))))
                  sym*))
               ; Expand body ...
               (expand! #f body* env cb-gensym)
               ; Find the last pair of the body
               (letrec* ((go-last (lambda (p)
                                    (let ((b (cdr p)))
                                      (cond
                                        ((null? b) p)
                                        ((pair? b) (go-last b))
                                        (else
                                          (error "dotted $extend-env??"
                                                 a))))))
                         (lastpair (go-last body*)))
                 ; Graft/Splice the body
                 (let ((d (cdr forms)))
                  (set-car! forms (car body*))
                  (set-cdr! forms (cdr body*))
                  (set-cdr! lastpair d)
                  (enter d)))))
            ((id-$define/primitive? i)
             ;; ($define/primitive sym LIB) -- not supported for now
             ;; ($define/primitive sym)
             (let ((sym (cadr a)))
              (envframe-add-binding! (env-current-frame env)
                                     sym
                                     (id-new-primitive sym sym #f)))
             (erase-next))
            ((id-$define-aux-syntax? i)
             ;; ($define-aux-syntax sym)
             (let ((sym (cadr a)))
              (envframe-add-binding! (env-current-frame env)
                                     sym
                                     (id-new-aux-syntax sym #f)))
             (erase-next))
            ((id-$inject? i)
             (expand-$inject #f #f forms a))
            ((id-$inject/splice? i)
             (expand-$inject #t #f forms a))
            ((id-$inject/multi? i)
             (expand-$inject #f #t forms a))
            ((id-$inject/form? i)
             (expand-$inject/form forms a))
            ((id-$alias? i)
             ;; ($alias sym1 sym2)
             (let ((sym1 (cadr a))
                   (sym2 (cadr (cdr a))))
               (let ((b (env-lookup env sym1)))
                (unless b
                  (error "cannot find id for alias" a))
                (envframe-add-binding! (env-current-frame env)
                                       sym2 b)))
             (erase-next))
            ((id-$quote? i)
             ;; ($quote obj)
             (set-car! forms (cadr a))
             (next))
            ((id-define-syntax? i)
             ;; (define-syntax name trans)
             (error "IMPLEMENT ME!")
             (next))
            ((id-syntax-rules? i)
             (error "syntax-rules outside define-syntax" a)
             (next))
            ((id-syntax-error? i)
             (error "Syntax error" a)
             (next))
            ;; Generic macro invocation
            ((id-macro? i)
             (error "IMPLEMENT ME!")
             (next))
            ;; Variable reference
            ((id-variable? i)
             (expand-car! #t a env)
             (re-enter))
            (else
              (error "Unknown identifier" a)))))
        (else
          ;; expand a as a single variable
          (expand-car! #f forms env)
          ;; go next
          (next)))))
    ((not (null? forms))
     (error "invalid top-level from (dotted-program???)" forms))))

(define (expand-forms! forms env cb-gensym)
  ;; Do nothing on null
  (cond
    ((pair? forms)
     (expand! #t forms env cb-gensym))
    ((not (null? forms))
     (error "invalid program format" forms))))
)
