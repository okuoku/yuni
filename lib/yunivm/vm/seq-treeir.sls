(library (yunivm vm seq-treeir)
         (export seq-treeir)
         (import (yuni scheme)
                 (yunivm vm vmcore))

;;

(define (seq-treeir heap ir)
  ;; Globals
  (define current-block #f)
  (define current-code #f)
  (define block-enters #f)
  (define block-breaks #f)
  (define block-siblings #f)
  (define jump-request #f)

  ;; Current VM
  (define vmcycle #f)
  (define vmextra #f)
  (define vmterm #f)

  ;; VM body
  (define (do-cycle)
    (cond
      ((null? current-code)
       (cond
         (current-block
           ;; Go to the next block
           (let* ((nextblock (vector-ref block-siblings current-block))
                  (nextcode (vector-ref block-breaks current-block)))
            (set! current-block nextblock)
            (set! current-code nextcode)
            (do-cycle)))
         (else
           ;; Terminate VM
           (vmterm))))
      ((pair? current-code)
       (let ((opline (car current-code))
             (nextcode (cdr current-code)))
         (cond
           ((pair? opline)
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
           ((vector? opline)
            ;; Debuginfo. Ignore.
            (set! current-code nextcode))
           (else
             (error "Unexpected opline" opline))))
       (do-cycle))
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
    (when (vm-true? obj)
      (jump label)))

  ;; Core libraries
  (define (query sym)
    (case sym
      ((CONSTANT HEAPIN HEAPOUT
                 GLOBAL MAKE-VMCLOSURE
                 MAKE-UNSPECIFIED
                 VM-ARGS-COMPOSE
                 VM-ARGS-DECOMPOSE
                 VM-PRIMITIVE?
                 VM-PRIMITIVE-ID
                 VM-PRIMITIVE-PROC
                 VM-CALL-ENV
                 VM-CALL-LABEL
                 HEAP-FRAME-SET!  HEAP-FRAME-REF HEAP-MAKE-FRAME
                 HEAP-FRAME-LENGTH HEAP-FRAME->LIST HEAP-LIST->FRAME
                 HEAP-CHAIN-LAST HEAP-CHAIN-LAST?  HEAP-CHAIN-CURRENT
                 HEAP-CHAIN-NEXT HEAP-CHAIN-CONS HEAP-CHAIN-REF
                 HEAP-HOST-KEY HEAP-HOST-FETCH) 
       ;; Re-export heap
       (heap sym))
      ((VM-RETURNPOINT)     vm-returnpoint)
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

  (define vmclosure? (heap 'VMCLOSURE?))
  (define vm-true? (heap 'VM-TRUE?))

  ;; Import heap interfaces
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
                      (let ((xvmterm (lambda () (extra 'RESULT #f #f))))
                       (set! vmcycle cycle)
                       (set! vmextra extra)
                       (set! vmterm xvmterm)
                       (do-cycle)))))
         
)
