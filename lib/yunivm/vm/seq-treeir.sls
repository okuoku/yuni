(library (yunivm vm seq-treeir)
         (export seq-treeir
                 seq-treeir-make-primitive)
         (import (yuni scheme)
                 (yunivm vm vmcore))

;;

(define *vmclosure-flag* (list '*vmclosure*))
(define *primitive-flag* (list '*primitive*))

;; Private
(define (vmclosure? obj)
  (and (pair? obj) (eq? *vmclosure-flag* (car obj))))
(define (make-primitive proc)
  (cons *primitive-flag* proc))

;; VM core support libraries
(define (constant imm) imm)
(define (make-closure label env)
  (cons *vmclosure-flag* (cons label env)))
(define (make-unspecified) 'unspecified)
(define (vm-args-compose . objs) objs)
(define (vm-args-decompose obj cb) (apply cb obj))
(define (vm-primitive? obj)
  (and (pair? obj) (eq? *primitive-flag* (car obj))))
(define (vm-callable obj) (cdr obj))
(define (vm-call-env obj)
  (cddr obj))
(define (vm-call-label obj)
  (cadr obj))


;; The Sequencer for Tree-IR
(define (seq-treeir-make-primitive proc)
  (make-primitive proc))

(define (seq-treeir global ir)
  ;; Globals
  (define current-block #f)
  (define current-code #f)
  (define block-enters #f)
  (define block-breaks #f)
  (define block-siblings #f)
  (define jump-request #f)

  ;; VM body
  (define (do-cycle vmcycle vmextra vmterm)
    (cond
      ((null? current-code)
       (cond
         (current-block
           ;; Go to the next block
           (let* ((nextblock (vector-ref block-siblings current-block))
                  (nextcode (vector-ref block-breaks current-block)))
            (set! current-block nextblock)
            (set! current-code nextcode)
            (do-cycle vmcycle vmextra vmterm)))
         (else
           ;; Terminate VM
           (vmterm))))
      ((pair? current-code)
       (let ((opline (car current-code))
             (nextcode (cdr current-code)))
         (let* ((op (car opline))
                (arg0 (and (pair? (cdr opline)) (cadr opline)))
                (arg1 (and (pair? (cdr opline))
                           (pair? (cddr opline))
                           (caddr opline))))
           (cond
             ((eq? 'block op)
              (set! current-block arg0)
              (set! current-code (cddr opline)))
             (else
               (set! jump-request #f)
               (vmcycle op arg0 arg1)
               (unless jump-request
                 ;; Go to the next inst
                 (set! current-code nextcode))))))
       (do-cycle vmcycle vmextra vmterm))
      (else
        (error "Invalid code fragment" current-code current-block))))

  ;; VM core support
  ;; Codeflow
  (define (vm-returnpoint) ;; => ('jump currentblock . target)
    (cons 'jump (cons current-block (cdr current-code))))
  (define (jump label)
    (set! jump-request #t)
    (let ((type (car label))
          (dest (cadr label)))
      (case type
        ((enter)
         (set! current-code (vector-ref block-enters dest))
         (set! current-block dest))
        ((break)
         (let ((nextblock (vector-ref block-siblings dest))
               (nextcode (vector-ref block-breaks dest)))
           (set! current-code nextcode)
           (set! current-block nextblock)))
        ((jump)
         (set! current-block dest)
         (set! current-code (cddr label)))
        (else
          (error "Invalid label type" label)))))
  (define (branch label obj)
    (when obj
      (jump label)))

  ;; Core libraries
  (define (query sym)
    (case sym
      ((CONSTANT)           constant)
      ((GLOBAL)             global)
      ((MAKE-CLOSURE)       make-closure)
      ((MAKE-UNSPECIFIED)   make-unspecified)
      ((VM-ARGS-COMPOSE)    vm-args-compose)
      ((VM-ARGS-DECOMPOSE)  vm-args-decompose)
      ((VM-PRIMITIVE?)      vm-primitive?)
      ((VM-CALLABLE)        vm-callable)
      ((VM-RETURNPOINT)     vm-returnpoint)
      ((VM-CALL-ENV)        vm-call-env)
      ((VM-CALL-LABEL)      vm-call-label)
      ((JUMP)               jump)
      ((BRANCH)             branch)
      (else (error "Invalid symbol for query" sym))))

  ; IR Tree walk helper
  (define (walk ir parent cb-block)
    (unless (null? ir)
      (let ((code (car ir)))
        (when (and (pair? code) (eq? 'block (car code)))
          (cb-block code parent (cdr ir))
          (walk (cddr code) (cadr code) cb-block))
       (walk (cdr ir) parent cb-block))))

  ;; Pass1: Scan for max-blockindex and allocate vector
  (let ((max-blockno 0))
   (walk ir #f
         (lambda (block parent next)
           (let ((no (cadr block)))
            (when (< max-blockno no)
              (set! max-blockno no)))))
   (let ((blockcount (+ 1 max-blockno)))
    (set! block-enters (make-vector blockcount #f))
    (set! block-breaks (make-vector blockcount #f))
    (set! block-siblings (make-vector blockcount #f))))
  ;; Pass2: Fill block pointers
  (walk ir #f
        (lambda (block parent next)
          (let ((no (cadr block)))
           (vector-set! block-enters no (cddr block))
           (vector-set! block-breaks no next)
           (vector-set! block-siblings no parent))))
  ;; Execute
  (set! current-code ir)
  (set! current-block #f)
  
  (call-with-values (lambda () (vmcore-new query))
                    (lambda (cycle extra) 
                      (let ((vmterm (lambda () (extra 'RESULT #f #f))))
                       (do-cycle cycle extra vmterm)))))
         
)
