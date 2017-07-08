(library (yunivm heap core)
         (export make-heap-core
                 gen-core-syms-vec)
         (import (yuni scheme)
                 (yunivm heap hostbridge)
                 (yunivm heap r7clib)
                 (yunivm util basiclibs)
                 ;; Gen-global-syms-vec
                 (yunivm util r7cmapping)
                 (yunivm util basiclibs)
                 (yunivm util compatlibs))

;; Tentative
(define (gen-core-syms-vec)
  ;; Merge r7cmapping/coreproc and basiclibs/compatlibs
  (let ((coresyms (apply append (map cdr r7cmapping/coreproc))))
   (list->vector
     (append coresyms 
             (vector->list basiclibs-name-vector)
             (vector->list compatlibs-name-vector)))))

;;
(define *vmclosure-flag* (list '*vmclosure*))
(define *primitive-flag* (list '*primitive*))

;; Private
(define (vmclosure? obj)
  (and (pair? obj) (eq? *vmclosure-flag* (car obj))))
(define (make-primitive obj)
  (cons *primitive-flag* obj))

;; VM core support libraries
(define (make-vmclosure label env)
  (cons *vmclosure-flag* (cons label env)))
(define (vm-primitive? obj)
  (or (procedure? obj)
      (and (pair? obj) (eq? *primitive-flag* (car obj)))))
(define (vm-call-env obj)
  (cddr obj))
(define (vm-call-label obj)
  (cadr obj))

(define (gen-global coreops global-syms-vec)
  (define r7c (make-r7clib coreops))
  (define symnames (vector->list global-syms-vec))
  (define vec
    (list->vector
      (map (lambda (sym)
             (make-primitive (r7c sym)))
           symnames)))
  (define (global mod idx)
    (unless (= mod 0)
      (error "Something wrong" mod idx))
    (vector-ref vec idx))

  global)

(define (make-heap-core coreops global-syms-vec)
  (define hostbridge (make-hostbridge coreops))
  (define host (hostbridge 'HOST))
  (define target (hostbridge 'TARGET))

  (define x-null? (coreops 'Pnull?))
  (define x-pair? (coreops 'Ppair?))
  (define co-null (coreops 'null))
  (define co-cons (coreops 'cons))
  (define co-car (coreops 'car))
  (define co-cdr (coreops 'cdr))

  ;; Non-recursive converter
  (define (targetargs lis)
    (cond
      ((null? lis) (co-null))
      ((pair? lis)
       (co-cons
         (car lis)
         (targetargs (cdr lis))))
      (else (error "Huh?"))))

  ;; Non-recursive converter
  (define (hostargs lis)
    (cond
      ((x-null? lis) '())
      ((x-pair? lis)
       (cons (co-car lis)
             (hostargs (co-cdr lis))))
      (else (error "Huh?"))))

  (define co-unspecified (coreops 'unspecified))

  (define (constant imm) 
    (error "Constant??" imm)
    (target imm))
  (define (vm-args-compose . objs) (targetargs objs))
  (define (vm-args-decompose obj cb) (apply cb (hostargs obj)))
  (define (make-unspecified) (co-unspecified))
  (define (vm-true? obj)
    ;; FIXME: Needs a bit more efficient way
    (if (host obj) #t #f))

  (define global (gen-global coreops global-syms-vec))

  (define (query sym)
    (case sym
      ((CONSTANT) constant)
      ((HEAPIN) target)
      ((HEAPOUT) host)
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
