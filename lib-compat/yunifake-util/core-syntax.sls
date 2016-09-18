(library (yunifake-util core-syntax)
         (export
           let
           let*
           do
           case
           cond
           and
           or
           quasiquote)
         (import (yunifake-util expander-callbacks))


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
       and
       ($$yunifake-expand-expr q)))))

(define-syntax or
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       or
       ($$yunifake-expand-expr q)))))

(define-syntax case
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       case
       ($$yunifake-expand-expr q)))))

(define-syntax cond
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       cond
       ($$yunifake-expand-expr q)))))

(define-syntax quasiquote
  (syntax-rules ()
    ((_ . q)
     ($$yunifake-inject-primitive
       quasiquote
       ($$yunifake-expand-expr q)))))

)
