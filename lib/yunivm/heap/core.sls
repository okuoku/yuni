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
  (define vmclosure? (coreops 'Pvmclosure?))
  (define make-primitive (coreops 'make-primitive))
  (define vm-primitive? (coreops 'Pprimitive?))
  (define vm-primitive-id (coreops 'primitive-id))
  (define make-vmclosure (coreops 'make-vmclosure))
  (define vm-call-env (coreops 'vmclosure-env))
  (define vm-call-label (coreops 'vmclosure-label))
  (define r7c (make-r7clib coreops))
  (define vec (make-vector (vector-length global-syms-vec)))
  (define procvec (make-vector (vector-length global-syms-vec)))

  (define (global mod idx)
    (unless (= mod 0)
      (error "Something wrong" mod idx))
    (vector-ref vec idx))
  (define (vm-primitive-proc prim)
    (vector-ref procvec (vm-primitive-id prim)))

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
    (if (or (vmclosure? obj) (host obj)) #t #f))

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
      ((VM-PRIMITIVE-ID) vm-primitive-id)
      ((VM-PRIMITIVE-PROC) vm-primitive-proc)
      ((VM-CALL-ENV) vm-call-env)
      ((VM-CALL-LABEL) vm-call-label)
      ((VM-TRUE?) vm-true?)
      (else (error "Invalid symbol for query" sym)) ))

  ;; Initialize globals
  (let loop ((idx 0))
   (unless (= idx (vector-length vec))
     (let* ((sym (vector-ref global-syms-vec idx))
            (id (case sym
                  ((apply) -1)
                  ((call-with-values) -2)
                  (else idx))))
       (vector-set! vec idx (make-primitive id))
       (vector-set! procvec idx (r7c sym))
       (loop (+ idx 1)))))

  query)         
         
)
