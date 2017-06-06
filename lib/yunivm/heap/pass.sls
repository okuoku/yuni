(library (yunivm heap pass)
         (export make-heap-pass)
         (import (yuni scheme))

         
;;
(define *vmclosure-flag* (list '*vmclosure*))
(define *primitive-flag* (list '*primitive*))

;; Private
(define (vmclosure? obj)
  (and (pair? obj) (eq? *vmclosure-flag* (car obj))))
(define (make-primitive obj)
  (cons *primitive-flag* obj))

;; VM core support libraries
(define (constant imm) imm)
(define (make-vmclosure label env)
  (cons *vmclosure-flag* (cons label env)))
(define (make-unspecified) 'unspecified)
(define (vm-args-compose . objs) objs)
(define (vm-args-decompose obj cb) (apply cb obj))
(define (vm-primitive? obj)
  (and (pair? obj) (eq? *primitive-flag* (car obj))))
(define (vm-call-env obj)
  (cddr obj))
(define (vm-call-label obj)
  (cadr obj))
(define (vm-true? obj)
  (if obj #t #f))

(define (make-heap-pass global-vars)
  (define global-vec
    (list->vector (map (lambda (e)
                         (if (procedure? e)
                           (make-primitive e)
                           e)) 
                       global-vars)))
  (define (global mod idx)
    (unless (= mod 0)
      (error "Something wrong" mod idx))
    (vector-ref global-vec idx))

  (define (query sym)
    (case sym
      ((CONSTANT) constant)
      ((GLOBAL) global)
      ((MAKE-VMCLOSURE) make-vmclosure)
      ((VMCLOSURE?) vmclosure?)
      ((MAKE-UNSPECIFIED) make-unspecified)
      ((VM-ARGS-COMPOSE) vm-args-compose)
      ((VM-ARGS-DECOMPOSE) vm-args-decompose)
      ((VM-PRIMITIVE?) vm-primitive?)
      ((VM-CALL-ENV) vm-call-env)
      ((VM-CALL-LABEL) vm-call-label)
      ((VM-TRUE?) vm-true?)
      (else (error "Invalid symbol for query" sym)) ))

  query)
         
)
