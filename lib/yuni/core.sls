(library (yuni core)
         (export ~ := 
                 define*
                 lambda*
                 let-with let-with*
                 is-a?
                 typeof
                 make touch!)
         (import (yuni scheme) 
                 (yuni miniobj)
                 (yuni miniobj minitype)
                 (yuni compat keywords))

; internal
(define-syntax ref
  (syntax-rules ()
    ((_ target slot)
     (miniobj-ref target slot))))

; internal
(define-syntax refset!
  (syntax-rules ()
    ((_ target slot value)
     (miniobj-set! target slot value))))

; ~: generic, recursive ref/set syntax.
(define-syntax-rules/keywords 
  ~ () (:=)
  ((_ target slot := obj)
   (refset! target slot obj))
  ((_ target slot)
   (ref target slot))
  ((_ target slot next-slot ...)
   (~ (ref target slot) next-slot ...)))

; define-composite
(define-syntax define-composite
  (syntax-rules ()
    ((_ typename slots)
     (define-minitype typename slots))))

; ~new
(define-syntax ~new
  (syntax-rules ()
    ((_ typename)
     (make-minitype-obj typename))))

; let-with
(define-syntax let-with
  (syntax-rules ()
    ((_ OBJ (specs0) body ...)
     (let-with-binder OBJ specs0 body ...))
    ((_ OBJ (specs0 specs1 ...) body ...)
     (let ((myobj OBJ))
       (let-with-binder myobj specs0
                        (let-with myobj (specs1 ...) body ...))))))

(define-syntax let-with*
  (syntax-rules ()
    ((_ (specs0 specs1 ...) body ...)
     (let-with specs0 (let-with* (specs1 ...) body ...)))
    ((_ (specs0) body ...)
     (let-with specs0 body ...))))

(define-syntax let-with-binder
  (syntax-rules ()
    ((_ OBJ (bindname name) body ...)
     (let ((bindname (~ OBJ 'name)))
       body ...))
    ((_ OBJ name body ...)
     (let ((name (~ OBJ 'name)))
       body ...))))

(define-syntax typeof
  (syntax-rules ()
    ((_ obj)
     (miniobj-typeof obj))))

(define-syntax is-a?
  (syntax-rules ()
    ((_ obj type)
     (and type (eq? type
                    (miniobj-typeof obj))))))

; make
(define-syntax make-apply-rule1!
  (syntax-rules ()
    ((_ NAME (slot body))
     (let ((result body))
       (~ NAME 'slot := result)))))

(define-syntax make
  (syntax-rules ()
    ((_ TYPE rule0 ...)
     (let ((new-object ((~new TYPE))))
       (make-apply-rule1! new-object rule0)
       ...
       new-object))))

(define-syntax touch!-bind-spec1
  (syntax-rules ()
    ((_ OBJ (slot) body ...)
     (begin body ...)) ; nothing to do (legacy form)
    ((_ OBJ (#f slot) body ...)
     (begin body ...)) ; nothing to do
    ((_ OBJ (bind slot) body ...)
     (let-with OBJ ((bind slot))
               body ...))
    ((_ OBJ slot body ...)
     (touch!-bind-spec1 OBJ (slot slot) body ...))))

(define-syntax touch!-bind-spec
  (syntax-rules ()
    ((_ OBJ (spec0) body ...)
     (touch!-bind-spec1 OBJ spec0 body ...))
    ((_ OBJ (spec0 spec1 ...) body ...)
     (touch!-bind-spec1 OBJ spec0
                        (touch!-bind-spec OBJ (spec1 ...) body ...)))))

(define-syntax touch!-apply-spec1!
  (syntax-rules ()
    ((_ OBJ (slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ (#f slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ (bind slot) body ...)
     (~ OBJ 'slot := body ...))
    ((_ OBJ slot body ...)
     (~ OBJ 'slot := body ...))))

(define-syntax touch!
  (syntax-rules ()
    ((_ OBJ (bind-spec0 body-spec0) ...)
     (let ((myobj OBJ))
       (touch!-bind-spec myobj (bind-spec0 ...)
                         (touch!-apply-spec1! myobj bind-spec0 body-spec0)
                         ...
                         myobj)))))


(define (type-check sym id-name type-name id type)
  (if (is-a? id type)
    'ok
    (begin ;; FIXME
      (error "yuni: type violation"
             id-name
             type-name))))

(define-syntax annotate-check
  (syntax-rules ()
    ((_ sym (id type))
     (type-check sym 'id 'type id type))
    ((_ sym id)
     'ok)))

(define-syntax raw-name
  (syntax-rules ()
    ((_ id) id)
    ((_ (id type)) id)))

(define-syntax lambda*0-itr
  (syntax-rules ()
    ;; Loops
    ((_ sym (cur ...) (spec ...) ((id bogus) rest0 ...) last body ...)
     (lambda*0-itr sym (cur ... id) ((id bogus) spec ...) (rest0 ...) last
                   body ...))
    ((_ sym (cur ...) (spec ...) ((id) rest0 ...) last body ...)
     (let ((lambda*0-proxy id))
       (lambda*0-itr sym (cur ...) (spec ...) ((id lambda*0-proxy) rest0 ...) 
                     last 
                     body ...)))
    ((_ sym (cur ...) (spec ...) (id rest0 ...) last body ...)
     (lambda*0-itr sym (cur ... id) (spec ...) (rest0 ...) last body ...))
    ;; Exit
    ((_ sym (cur ...) (spec ...) () () body ...) ;; Non multi case
     (lambda (cur ...) 
       (annotate-check sym spec) ...
       (let () body ...)))  
    ;; Exit (multi/type case)
    ((_ sym (cur ...) (spec ...) () (last-id last-type) body ...)
     (lambda (cur ... . last-id)
       (for-each (lambda (e) (annotate-check e last-type)) last-id)
       (annotate-check sym spec) ...
       (let () body ...)))
    ;; Exit (multi w/o type case)
    ((_ sym (cur ...) (spec ...) () last body ...)
     (lambda (cur ... . last) 
       (annotate-check sym spec) ...
       (let () body ...)))))

;; Process type specifier
(define-syntax lambda*0
  (syntax-rules ()
    ((_ sym (spec0 ...) last body ...)
     (lambda*0-itr sym () () (spec0 ...) last body ...))))

;; Process property specifier
(define (lookup-property form sym def)
  (define (fail) (values def form))
  (define (itr ret top next rest)
    ;(write (list 'itr: ret top next rest))(newline)
    (if (eq? sym top)
      (values next (append (reverse ret) rest))
      (if (pair? rest) 
        (itr (cons top ret) next (car rest) (cdr rest))
        (fail))))
  ;(write (list 'in: form sym))(newline)
  (if (and (pair? form)
           (pair? (cdr form))) 
    (itr '() (car form) (cadr form) (cddr form))
    (fail)))

(define-syntax let-property
  (syntax-rules ()
    ((_ form out #() proc)
     (let ((out form)) proc))
    ((_ form out #(sym) proc)
     (let-property form out #(sym #f) proc))
    ((_ form out #(sym def) proc)
     (let-values (((sym out) (lookup-property form 'sym def)))
                 proc))))

(define-syntax let-properties
  (syntax-rules ()
    ((_ form () proc)
     (apply proc form))
    ((_ form (props0 props1 ...) proc)
     (let-property form out props0
                   (let-properties out (props1 ...) proc)))))

(define-syntax flatten-properties-itr
  (syntax-rules ()
    ((_ form (out ...) (#((obj ...) rest ...) next ...) proc)
     (flatten-properties-itr
       form (out ... #(obj ...)) (#(rest ...) next ...) proc))
    ((_ form (out ...) (next0 next1 ...) proc)
     (flatten-properties-itr
       form (out ... next0) (next1 ...) proc))
    ((_ form (out ...) () proc)
     (let-properties form (out ...) proc))))

(define-syntax flatten-properties
  (syntax-rules ()
    ((_ form (obj ...) proc)
     (flatten-properties-itr form () (obj ...) proc))))

(define-syntax lambda*1-itr
  (syntax-rules ()
    ;; Loops
    ((_ sym (prop ...) (out ...) (#(propx ...) spec0 ...) last body ...)
     (lambda*1-itr sym (#(propx ...) prop ...) (out ...) (spec0 ...) last 
                   body ...))
    ((_ sym (prop ...) (out ...) (spec0 spec1 ...) last body ...)
     (lambda*1-itr sym (prop ...) (out ... spec0) (spec1 ...) last body ...))

    ;; Exit
    ;; Shortcut (No properties)
    ((_ sym () (out ...) () last body ...)
     (lambda*0 sym (out ...) last body ...))
    ;; Handle properties
    ((_ sym (prop0 ...) (out ...) () last body ...)
     (lambda property-input 
       (flatten-properties property-input (prop0 ...) 
                           (lambda*0 sym (out ...) last body ...))))))

(define-syntax lambda*1
  (syntax-rules ()
    ((_ sym (spec0 ...) body ...)
     (lambda*1-itr sym () () (spec0 ...) () body ...))
    #| Issue #26
    ((_ sym (spec0 ... . last) body ...)
     (lambda*1-itr sym () () (spec0 ...) last body ...))
    |#
    ))

;; Yuni core syntax entry point
(define-syntax lambda*
  (syntax-rules ()
    ((_ spec body ...)
     (lambda*1 'lambda spec body ...))))

;; Yuni core syntax entry point
(define-syntax define*
  (syntax-rules ()
    ((_ (name . spec) body ...)
     (define name (lambda*1 'name spec body ...)))
    ((_ name spec)
     (define-composite name spec))))

(define-keywords :=)

)
