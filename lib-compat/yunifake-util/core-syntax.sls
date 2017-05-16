(library (yunifake-util core-syntax)
         (export
           let
           let*
           do
           case
           cond
           and
           or
           quasiquote
           ;define-vaules
           )
         (import (yunifake-util expander-callbacks))


#|
(define-syntax $$define-values/dotted-itr
  (syntax-rules ()
    ((_ (frms ...) (v0 v1 v2 ...) expr)
     ($$define-values/dotted-itr (frms ... v0) (v1 v2 ...) expr))
    ((_ (frms ...) (last) expr)
     ($$yunifake-inject-primitive/raw
       define-values
       .
       ((frms ... . last) expr)))))

(define-syntax $$define-values/remap-dotted
  (syntax-rules ()
    ((_ () () (vars expr))
     ($$define-values/dotted-itr
       ()
       vars
       expr))))

(define-syntax $$define-values/remap
  (syntax-rules ()
    ((_ () () body)
     ($$yunifake-inject-primitive/raw 
       define-values
       .
       body))))

(define-syntax $$define-values/emit
  (syntax-rules ()
    ((_ remap (frms ...) last expr)
     (begin
       ($$yunifake-inject-definition frms #f)
       ...
       ($$yunifake-inject-definition
         last
         ($$yunifake-bind
           remap () ()
           (frms ... last) expr))))
    ((_ remap #f var expr)
     ($$yunifake-inject-definition
       var
       ($$yunifake-bind
         remap () ()
         var expr))))) 

(define-syntax $$define-values/construct
  (syntax-rules ()
    ((_ (acc ...) (frm . (next . out)) expr)
     ($$define-values/construct
       (acc ... frm)
       (next . out)
       expr))
    ((_ (acc ...) (frm) expr)
     ($$define-values/emit
       $$define-values/remap
       (acc ...)
       frm
       expr))
    ((_ (acc ...) (frm . rest) expr)
     ($$define-values/emit
       $$define-values/remap-dotted
       (acc ... frm)
       rest
       expr))
    ((_ () frm expr)
     ($$define-values/emit
       $$define-values/remap
       #f
       frm
       expr))))

(define-syntax define-values
  (syntax-rules ()
    ((_ frm expr)
     ($$define-values/construct
       () ;; acc
       frm expr))))
|#

(define-syntax $$let/remap
  (syntax-rules ()
    ((_ (vars ()) () . cmd)
     ($$yunifake-inject-primitive/raw let vars . cmd))
    ((_ (vars (init0 . init*))  (var0 . var*) . cmd)
     ($$let/remap (((var0 init0) . vars) init*) var* . cmd))))

(define-syntax let
  (syntax-rules ()
    ((_ ((var init) ...) . body)
     ($$yunifake-bind/body
       $$let/remap
       (() (init ...))
       (var ...)
       . body))
    ((_ name ((var init) ...) . body) ;; named-let case
     (let ()
      (define (name var ...) . body)
      (name init ...)))))
         
(define-syntax let*
  (syntax-rules ()
    ((_ () . body)
     (let () . body))
    ((_ (first . next) . body)
     (let (first)
      (let* next . body)))))
         
(define-syntax $$do/remap
  (syntax-rules ()
    ((_ (vars ()) () (() . cmd))
     ($$yunifake-inject-primitive/raw do vars . cmd))
    ((_ (vars (init0 . init1)) (var0 . var1) ((() . step1) . cmd))
     ($$do/remap (((var0 init0) . vars) init1) var1 (step1 . cmd)))
    ((_ (vars (init0 . init1)) (var0 . var1) (((step0) . step1) . cmd))
     ($$do/remap (((var0 init0 step0) . vars) init1) var1 (step1 . cmd))))) 

(define-syntax do
  (syntax-rules ()
    ((_ ((var init step ...)
         ...)
        .
        test+cmd)
     ($$yunifake-bind
       $$do/remap
       (() (init ...))
       (var ...)
       ((step ...) ...)
       .
       test+cmd))))

(define-syntax and
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       and q))))

(define-syntax or
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       or q))))

(define-syntax $$case/clause
  (syntax-rules ()
    ((_ (top . q))
     ($$yunifake-inject-primitive top q))))

(define-syntax $$case/remap
  (syntax-rules ()
    ((_ code (Q ...) clause0 rest ...)
     ($$case/remap code
                   (Q ... ($$case/clause clause0)) rest ...))
    ((_ code Q)
     ($$yunifake-inject-primitive
       case (code . Q)))))

(define-syntax case
  (syntax-rules ()
    ((_ code clauses ...)
     ($$case/remap code () clauses ...))))

(define-syntax cond
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       cond q))))

(define-syntax $$yunifake-qq
  (syntax-rules ($$yunifake-qq quasiquote unquote unquote-splicing)
    ;; Depth == 0
    ((_ (unquote input)) input)
    ((_ ((unquote-splicing x) . y))
     ;; Reenter
     (append x ($$yunifake-qq y)))

    ;; Outputmyself1
    ((_ (quasiquote x) . depth)
     ;; Depth++
     (cons 'quasiquote ($$yunifake-qq (x) depth)))
    ;; Outputmyself2
    ((_ ($$yunifake-qq x) . depth)
     ;; Depth++
     (cons 'quasiquote ($$yunifake-qq (x) depth)))

    ;; Outputunquote
    ((_ (unquote x) depth)
     ;; Depth--
     (cons 'unquote ($yunifake-qq (x) . depth)))
    ;; Outputunquote-splicing
    ((_ (unquote-splicing x) depth)
     (cons 'unquote-splicing ($$yunifake-qq (x) . depth)))

    ;; Outputcons
    ((_ (a . d) . depth)
     ;; Depth no change
     (cons ($$yunifake-qq a . depth)
           ($$yunifake-qq d . depth)))

    ;; Outputvector
    ((_ #(lis ...) . depth)
     ;; Depth no change
     (list->vector ($$yunifake-qq (lis ...) . depth)))

    ;; Terminate
    ((_ input . any) 'input)))

(define-syntax quasiquote ;; Depth zero, entrypoint
  (syntax-rules ()
    ;; Reject (quasiquote 1 2 ...) case
    ((_ input)
     ($$yunifake-qq input))))

#|
(define-syntax quasiquote
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       quasiquote q))))
|#

)
