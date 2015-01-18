(library (yuni ffi database functions)
         (export
           make-functions
           functions-procedures
           functions-add-procedure!
           
           make-procedure
           procedure-add-argument!
           procedure-arguments
           procedure-stub-types
           procedure-add-stub-type!
           
           make-argument
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

(define* (functions-procedures (functions))
  (~ functions 'procedures*))

(define* (functions-add-procedure! (functions) (procedure))
  (define s (functions-procedures functions))
  (~ functions 'procedures* := (cons procedure s)))

(define (make-procedure return-type name)
  (make procedure
        (name name)
        (args* (list return-type))
        (stub-types* '())))

(define* (procedure-add-argument! (procedure) (argslot))
  ;; NB: We must keep order. Use APPEND.
  (define s (procedure-arguments procedure))
  (~ procedure 'args* := (append s (list argslot))))

(define* (procedure-arguments (procedure))
  (~ procedure 'args*))

(define* (procedure-stub-types (procedure))
  (~ procedure 'stub-types*))

(define* (procedure-add-stub-type! (procedure) sym)
  (define s (procedure-stub-types procedure))
  (~ procedure 'stub-types* := (cons sym s)))


(define (make-argument c-type name)
  (make argslot
        (name name)
        (input? #f)
        (output? #f)
        (c-type c-type)
        (type #f)
        (constraints* '())))

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
