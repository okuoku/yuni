(library (yunivm vm vmcore)
         (export vmcore-new)
         (import (yuni scheme))
         
;;

(define (vmcore-new imm-tbl query) ;; => cycle extra
  ;; Machine state
  (define S* '())
  (define S #f)
  (define E* '())
  (define E #f)
  (define D* '())
  (define V '())
  (define IMMFRAME #f)
  (define link 'none) ;; none | single | values | call | multi

  ;; Query(call-back)
  (define constant          (query 'CONSTANT))        ;; (imm)
  (define global            (query 'GLOBAL))          ;; (mod pos)
  (define heapin            (query 'HEAPIN))
  (define heapout           (query 'HEAPOUT))
  (define make-closure      (query 'MAKE-VMCLOSURE))  ;; (label env)
  (define make-unspecified  (query 'MAKE-UNSPECIFIED))
  (define vm-args-compose   (query 'VM-ARGS-COMPOSE))      ;; objs
  (define vm-args-decompose (query 'VM-ARGS-DECOMPOSE))    ;; (obj cb)
  (define vm-primitive?     (query 'VM-PRIMITIVE?))        ;; (obj)
  (define vm-primitive-id   (query 'VM-PRIMITIVE-ID))
  (define vm-primitive-proc (query 'VM-PRIMITIVE-PROC))
  (define vm-returnpoint    (query 'VM-RETURNPOINT))
  (define vm-call-env       (query 'VM-CALL-ENV))          ;; (obj)
  (define vm-call-label     (query 'VM-CALL-LABEL))        ;; (obj)
  (define jump              (query 'JUMP))            ;; (label)
  (define branch            (query 'BRANCH))          ;; (label obj)
  (define set-gc-hook!      (query 'HEAP-SET-GC-HOOK!))
  (define gc-mark!          (query 'HEAP-GC-MARK!))

  (define frame-set! (query 'HEAP-FRAME-SET!))
  (define frame-ref (query 'HEAP-FRAME-REF))
  (define make-frame (query 'HEAP-MAKE-FRAME))
  (define frame-length (query 'HEAP-FRAME-LENGTH))
  (define frame->list (query 'HEAP-FRAME->LIST))
  (define list->frame (query 'HEAP-LIST->FRAME))
  (define chain-last (query 'HEAP-CHAIN-LAST))
  (define chain-last? (query 'HEAP-CHAIN-LAST?))
  (define chain-current (query 'HEAP-CHAIN-CURRENT))
  (define chain-next (query 'HEAP-CHAIN-NEXT))
  (define chain-cons (query 'HEAP-CHAIN-CONS))
  (define chain-ref (query 'HEAP-CHAIN-REF))
  (define hostkey (query 'HEAP-HOST-KEY))
  (define hostfetch (query 'HEAP-HOST-FETCH))

  (define (reset-vm-state!)
    (set! S #f)
    (set! S* (chain-last))
    (set! E #f)
    (set! E* (chain-last))
    (set! D* (chain-last))
    (set! V #f)
    (set! link 'none)
    (set! IMMFRAME (make-frame (vector-length imm-tbl)))
    (set-gc-hook! gc-hook)
    (gc-disable)
    ;; Fill imm table
    (let loop ((idx 0))
     (unless (= idx (vector-length imm-tbl))
       (frame-set! IMMFRAME idx (heapin (vector-ref imm-tbl idx)))
       (loop (+ idx 1))))
    (gc-enable))
  
  (define (gc-enable) (set-gc-hook! #t))
  (define (gc-disable) (set-gc-hook! #f))

  (define (gc-hook)
    (when S
      (gc-mark! S))
    (gc-mark! S*)
    (when E
      (gc-mark! E))
    (gc-mark! E*)
    (gc-mark! D*)
    (gc-mark! IMMFRAME)
    ;; FIXME: Assumes fixnum heap here....
    (when V
      (gc-mark! V)))

  ;; Register stack
  (define (push-S! vec)
    (let ((curS S))
     (set! S vec)
     (when curS
       (set! S* (chain-cons curS S*)))))
  (define (pop-S!)
    (cond
      ((chain-last? S*)
       (unless S
         (error "Stack underflow(S)"))
       (set! S #f))
      (else
        (let ((a (chain-current S*))
              (d (chain-next S*)))
          (set! S a)
          (set! S* d)))))
  (define (push-E! vec)
    (let ((curE E))
     (set! E vec)
     (when curE
       (set! E* (chain-cons curE E*)))))
  (define (pop-E!)
    (cond
      ((chain-last? E*)
       (unless E
         (error "Stack underflow(E)"))
       (set! E #f))
      (else
        (let ((a (chain-current E*))
              (d (chain-next E*)))
          (set! E a)
          (set! E* d)))))
  (define (apply-env! obj)
    (let ((top (chain-current obj))
          (next (chain-next obj)))
      (set! E top)
      (set! E* next)
      (set! S* (chain-last))))

  ;; OP macros
  (define (set-value! val)
    (set! V val)
    (set! link 'single))
  
  ;; OPs
  ;; Env frame
  (define (FRAME imm)
    (let ((v (make-frame imm)))
     (push-S! v)))
  (define (RECV imm)
    (define stack-len (frame-length S))
    ;(pp (list 'RECV: S))
    (case link
      ((call)
       (unless (= stack-len imm)
         (error "Argument count unmatch" S imm))
       (push-E! S))
      ((multi)
       (when (= 0 stack-len)
         (error "Null stack?? (FIXME)"))
       (when (< imm stack-len)
         (error "Argument count unmatch" S imm))
       (let ((vec1 (make-frame imm))
             (rest (frame-ref S (- stack-len 1))))
         (vm-args-decompose 
           rest
           (lambda objs
             (let loop ((idx 0)
                        (cur objs))
               (cond
                 ((< idx (- stack-len 1))
                  (frame-set! vec1 idx (frame-ref S idx))
                  (loop (+ idx 1) cur))
                 (else
                   (frame-set! vec1 idx (car cur))
                   (unless (null? (cdr cur))
                     (loop (+ idx 1) (cdr cur))))))))
         (push-E! vec1)))
      (else
        (error "Invalid link status" link))))
  (define (RECVM imm)
    (define stack-len (frame-length S))
    ;(pp (list 'RECVM: S))
    (case link
      ((call)
       (unless (<= imm stack-len)
         (error "Argument count unmatch" S imm))
       ;; We have to use native pair to hold temporal values
       ;; Because we cannot hold values to VM register prevent GCs
       (let loop ((idx 0)
                  (cur1 '())
                  (cur2 '()))
         (cond
           ((< idx imm)
            ;; Collect arg1
            (loop (+ idx 1) (cons (frame-ref S idx) cur1) cur2))
           ((= stack-len idx)
            ;; Push to E
            (push-E! (list->frame (reverse (cons (apply vm-args-compose
                                                        (reverse cur2))
                                                 cur1)))) )
           (else
             ;; Collect arg2
             (loop (+ idx 1) cur1 (cons (frame-ref S idx) cur2))))))
      ((multi)
       (cond
         ((= stack-len (+ imm 1))
          ;; Short cut
          (push-E! S))
         (else
           ;; Slow path
           (let* ((l (reverse (frame->list S)))
                  (resto (car l))
                  (args0 (reverse (cdr l))))
             (vm-args-decompose
               resto
               (lambda objs
                 (let ((args (append args0 objs)))
                  (unless (<= imm (length args))
                    (error "Argument count unmatch" S imm))
                  (let loop ((idx 0)
                             (cur '())
                             (rest args))
                    (cond
                      ((< idx imm)
                       (when (null? rest)
                         (error "Argument count unmatch??? (FIXME)"))
                       (loop (+ idx 1) (cons (car rest) cur) (cdr rest)))
                      (else
                        (push-E! (list->frame 
                                   (reverse (cons (apply vm-args-compose rest)
                                                  cur))))))))))))))
      (else
        (error "Invalid link status" link)))) 
  (define (BIND)
    (push-E! S)
    (pop-S!))
  (define (LEAVE)
    (pop-E!))

  ;; Branch/Call
  (define (prepare-args type stack)
    (case type
      ((call) (frame->list stack))
      ((multi)
       (let ((l (frame->list stack)))
        (cond
          ((null? l) '())
          (else
            (let* ((a0 (reverse l))
                   (rest (car a0))
                   (args (reverse (cdr a0))))
              (vm-args-decompose
                rest
                (lambda objs
                  (append args objs))))))))
      (else
        (error "Invalid call type" type))))

  (define (call-primitive! type)
    (let ((l (prepare-args type S))
          (id (vm-primitive-id V)))
      ;; Save current stack content to V to prevent premature GC collection
      (case id
        ((-1) ;; apply
         (set-value! S)
         (pop-S!)
         (unless (< 0 (length l))
           (error "Invalid parameter for apply" l))
         (let ((proc (car l))
               (args (cdr l)))
           (let ((flen (length args)))
            (FRAME flen)
            (let loop ((idx 0)
                       (x args))
             (unless (= idx flen)
               (set-value! (car x))
               (MOV idx)
               (loop (+ idx 1) (cdr x))))
            (set-value! proc)
            (TCALLM))))
        ((-2) ;; call-with-values
         (set-value! S)
         (pop-S!)
         (unless (= (length l) 2)
           (error "Invalid parameter for call-with-values" l))
         (let ((send (car l))
               (recv (cadr l)))
           (save-dump-obj!
             recv
             (lambda (ctx)
               ;; Load values
               (case link
                 ((values) 
                  (push-S! V))
                 ((single)
                  (FRAME 1)
                  (MOV 0))
                 ((none)
                  (FRAME 0))
                 (else
                   (error "Invalid link state for call-with-values" link)))
               ;; Load receiver
               (set-value! ctx)
               ;; Call receiver
               (TCALL)))
           ;; Push nothing
           (FRAME 0)
           ;; Load sender
           (set-value! send)
           ;; Call sender
           (TCALL)))
        (else ;; Standard primitives
          (let ((proc V)) ;; FIXME: We shouldn't need this as we 
                          ;;        do not have callbacks any longer
           (set-value! S)
           (pop-S!)
           (call-with-values
             (lambda () 
               (apply (vm-primitive-proc proc) l))
             (lambda vals
               ;(pp (list 'RESULT: vals))
               (case (length vals)
                 ((0) (set! V #f)
                      (set! link 'none))
                 ((1) (set! V (car vals))
                      (set! link 'single))
                 (else (set! V (list->frame vals))
                       (set! link 'values)))
               (restore-dump!))))))))
  (define (call-label! type)
    (set! link type)
    (apply-env! (vm-call-env V))
    (jump (hostfetch (vm-call-label V))))
  (define (save-dump-obj! ctx obj)
    (set! D* (chain-cons (list->frame 
                           (if ctx
                             (list S* E E* (hostkey obj) ctx)
                             (list S* E E* (hostkey obj)))) 
                         D*)))
  (define (save-dump!)
    (save-dump-obj! #f (vm-returnpoint)))
  (define (restore-dump!)
    (let ((a (chain-current D*))
          (d (chain-next D*)))
      (apply (lambda (ex-S* ex-E ex-E* r . ctx?)
               (let ((returnpoint (hostfetch r)))
                (set! S '())
                (set! S* ex-S*)
                (pop-S!)
                (set! E ex-E)
                (set! E* ex-E*)
                (cond
                  ;; Callbacks will use procedure as returnpoint
                  ((procedure? returnpoint) (returnpoint (car ctx?)))
                  (else (jump returnpoint)))))
             (frame->list a))
      (set! D* d)))
  (define (call-dispatch! type tail?)
    (unless (eq? link 'single)
      (error "Invalid object for call" V))
    (let ((primitive? (vm-primitive? V)))
     (unless tail?
       (save-dump!))
     (cond
       (primitive?
         (call-primitive! type))
       (else
         (call-label! type)))))
  (define (CALL)
    (call-dispatch! 'call #f))
  (define (CALLM)
    (call-dispatch! 'multi #f))
  (define (TCALL)
    (call-dispatch! 'call #t))
  (define (TCALLM)
    (call-dispatch! 'multi #t))

  (define (RET)
    (restore-dump!))
  (define (JMP label)
    (jump label))
  (define (BRV label)
    (unless (eq? link 'single)
      (error "Invalid object for branch" V))
    (branch label V))

  ;; Load/Store
  (define (LD frm pos)
    (cond
      ((= frm 0)
       (set-value! (frame-ref E pos)))
      (else
        (let* ((offs (- frm 1))
               (vec (chain-ref E* offs)))
          (set-value! (frame-ref vec pos))))))
  (define (LDG mod pos)
    (set-value! (global mod pos)))
  (define (LDF label)
    (let* ((env (chain-cons E E*))
           (key (hostkey label)))
      ;; chain-cons can cause GC
      (set-value! (make-closure key env))))
  (define (LDC cnr)
    (set-value! (constant cnr)))
  (define (LDV)
    (case (frame-length S)
      ((1) (set! V (frame-ref S 0))
           (set! link 'single))
      ((0) (set! V #f)
           (set! link 'none))
      (else
        (set! V S)
        (set! link 'values)))
    (pop-S!))
  (define (LDI imm)
    (set-value! (frame-ref IMMFRAME imm)))
  (define (LDN)
    (set-value! (make-unspecified)))
  (define (LDNN)
    (set! link 'none))

  (define (ST frm pos)
    (unless (eq? link 'single)
      (error "Invalid link status" link))
    (cond
      ((= frm 0)
       (frame-set! E pos V))
      (else
        (let* ((offs (- frm 1))
               (vec (chain-ref E* offs)))
          (frame-set! vec pos V)))))
  (define (MOV loc)
    (unless (eq? link 'single)
      (error "Invalid link status" link))
    (frame-set! S loc V))
  
  ;; Machine cycle
  (define (cycle op arg0 arg1)
    ;(define (clean-V v)
    ;  (if (pair? v)
    ;    "pair-or-something..."
    ;    v))
    ;(write (list 'cycle: op arg0 arg1 (list 'V: (clean-V V)))) (newline)
    ;(gc-disable)
    (case op
      ((FRAME)  (FRAME arg0))
      ((RECV)   (RECV  arg0))
      ((RECVM)  (RECVM arg0))
      ((BIND)   (BIND))
      ((LEAVE)  (LEAVE))
      ((CALL)   (CALL))
      ((CALLM)  (CALLM))
      ((TCALL)  (TCALL))
      ((TCALLM) (TCALLM))
      ((RET)    (RET))
      ((JMP)    (JMP   arg0))
      ((BRV)    (BRV   arg0))
      ((LD)     (LD    arg0 arg1))
      ((LDG)    (LDG   arg0 arg1))
      ((LDF)    (LDF   arg0))
      ((LDC)    (LDC   arg0))
      ((LDV)    (LDV))
      ((LDI)    (LDI   arg0))
      ((LDN)    (LDN))
      ((LDNN)   (LDNN))
      ((ST)     (ST    arg0 arg1))
      ((MOV)    (MOV   arg0))
      (else
        (error "Invalid opcode" (list op arg0 arg1))))
    ;(gc-enable)
    )

  ;; Extra query
  (define (extra op arg0 arg1)
    (case op
      ((RESULT)
       (case link
         ((single) (heapout V))
         ((values) (apply values (map heapout (frame->list V))))
         ((none) (values))
         (else
           (error "No result available" link V))))
      (else
        (error "Invalid extra request" (list op arg0 arg1)))) )

  (reset-vm-state!)
  (values cycle extra))
         
)
