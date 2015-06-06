(library (yuni ffi database functions)
         (export
           make-functions
           functions-entries
           functions-add-entry!
           
           make-function
           function-name
           function-return-argument
           function-add-argument!
           function-arguments
           function-stub-types
           function-add-stub-type!
           
           make-argument
           argument-name
           argument-type
           argument-c-type
           argument-type-set!
           argument-constraints
           argument-add-constraint!
           argument-input?
           argument-output?
           argument-input?-set!
           argument-output?-set!
           )
         (import (yuni scheme)
                 (yuni base match)
                 (yuni core))

;; functions
         
(define* functions (procedures*))         

(define* procedure
  (name
    retarg
    args* ;; (argslot ...)
    stub-types*
    ))

(define* argslot
  (name ;; => #f | sym
    input?
    output?
    c-type ;; => (typeish ...) 
    type ;; => #f | type
    constraints*
    ))

(define (make-functions)
  (make functions
        (procedures* '())))

(define* (functions-entries (functions))
  (~ functions 'procedures*))

(define* (functions-add-entry! (functions) (procedure))
  (define s (functions-entries functions))
  (~ functions 'procedures* := (cons procedure s)))

(define (make-function return-type name)
  (make procedure
        (name name)
        (retarg return-type)
        (args* '())
        (stub-types* '())))

(define* (function-add-argument! (procedure) (argslot))
  ;; NB: We must keep order. Use APPEND.
  (define s (function-arguments procedure))
  (~ procedure 'args* := (append s (list argslot))))

(define* (function-arguments (procedure))
  (~ procedure 'args*))

(define* (function-stub-types (procedure))
  (~ procedure 'stub-types*))

(define* (function-add-stub-type! (procedure) sym)
  (define s (function-stub-types procedure))
  (~ procedure 'stub-types* := (cons sym s)))

(define* (function-name (procedure))
  (~ procedure 'name))

(define* (function-return-argument (procedure))
  (~ procedure 'retarg))

(define (make-argument c-type name)
  (make argslot
        (name name)
        (input? #f)
        (output? #f)
        (c-type c-type)
        (type #f)
        (constraints* '())))

(define* (argument-name (argslot))
  (~ argslot 'name))

(define* (argument-type-set! (argslot) sym)
  (~ argslot 'type := sym))

(define* (argument-type (argslot))
  (~ argslot 'type))

(define* (argument-c-type (argslot))
  (~ argslot 'c-type))

(define* (argument-add-constraint! (argslot) c)
  (define s (argument-constraints argslot))
  (~ argslot 'constraints* := (cons c s)))

(define* (argument-constraints (argslot))
  (~ argslot 'constraints*))

(define* (argument-input? (argslot))
  (~ argslot 'input?))

(define* (argument-output? (argslot))
  (~ argslot 'output?))

(define* (argument-input?-set! (argslot) b)
  (~ argslot 'input? := b))

(define* (argument-output?-set! (argslot) b)
  (~ argslot 'output? := b))


)
