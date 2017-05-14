(library (yunivm vm vmcore)
         (export vmcore-new)
         (import (yuni scheme))
         
;;

(define (vmcore-new query) ;; => cycle extra
  ;; Machine state
  (define S* '())
  (define S #f)
  (define E* '())
  (define E #f)
  (define D* '())
  (define V '())
  (define link 'none) ;; none | single | values | call | multi

  ;; Query(call-back)
  (define constant          (query 'CONSTANT))        ;; (imm)
  (define global            (query 'GLOBAL))          ;; (mod pos)
  (define make-closure      (query 'MAKE-CLOSURE))    ;; (label env)
  (define make-unspecified  (query 'MAKE-UNSPECIFIED))
  (define vm-args-compose   (query 'VM-ARGS-COMPOSE))      ;; objs
  (define vm-args-decompose (query 'VM-ARGS-DECOMPOSE))    ;; (obj cb)
  (define vm-primitive?     (query 'VM-PRIMITIVE?))        ;; (obj)
  (define vm-callable       (query 'VM-CALLABLE))          ;; (obj)
  (define vm-returnpoint    (query 'VM-RETURNPOINT))
  (define vm-call-env       (query 'VM-CALL-ENV))          ;; (obj)
  (define vm-call-label     (query 'VM-CALL-LABEL))        ;; (obj)
  (define jump              (query 'JUMP))            ;; (label)
  (define branch            (query 'BRANCH))          ;; (label obj)

  ;; Register stack
  (define (push-S! vec)
    (when S
      (set! S* (cons S S*)))
    (set! S vec))
  (define (pop-S!)
    (cond
      ((null? S*)
       (unless S
         (error "Stack underflow(S)"))
       (set! S #f))
      (else
        (let ((a (car S*))
              (d (cdr S*)))
          (set! S a)
          (set! S* d)))))
  (define (push-E! vec)
    (when E
      (set! E* (cons E E*)))
    (set! E vec))
  (define (pop-E!)
    (cond
      ((null? E*)
       (unless E
         (error "Stack underflow(E)"))
       (set! E #f))
      (else
        (let ((a (car E*))
              (d (cdr E*)))
          (set! E a)
          (set! E* d)))))
  (define (apply-env! obj)
    (let ((top (car obj))
          (next (cdr obj)))
      (set! E top)
      (set! E* next)
      (set! S* '())))

  ;; OP macros
  (define (set-value! val)
    (set! V val)
    (set! link 'single))
  
  ;; OPs
  ;; Env frame
  (define (FRAME imm)
    (let ((v (make-vector imm #f)))
     (push-S! v)))
  (define (RECV imm)
    (define stack-len (vector-length S))
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
       (let ((vec1 (make-vector imm))
             (rest (vector-ref S (- stack-len 1))))
         (vm-args-decompose 
           rest
           (lambda objs
             (let loop ((idx 0)
                        (cur objs))
               (cond
                 ((< idx (- stack-len 1))
                  (vector-set! vec1 idx (vector-ref S idx))
                  (loop (+ idx 1) cur))
                 (else
                   (vector-set! vec1 idx (car cur))
                   (unless (null? (cdr cur))
                     (loop (+ idx 1) (cdr cur))))))))
         (push-E! vec1)))
      (else
        (error "Invalid link status" link))))
  (define (RECVM imm)
    (define stack-len (vector-length S))
    (case link
      ((call)
       (unless (<= imm stack-len)
         (error "Argument count unmatch" S imm))
       (let ((vec1 (make-vector (+ 1 imm)))
             (vec2 (make-vector (- stack-len imm))))
        (let loop ((idx 0))
         (cond
           ((< idx imm)
            (vector-set! vec1 idx 
                         (vector-ref S idx))
            (loop (+ idx 1)))
           (else
             (vector-set! vec2 (- idx imm)
                          (vector-ref S idx))
             (unless (= stack-len idx)
               (loop (+ idx 1)))))
         (vector-set! vec1 imm
                      (apply vm-args-compose
                             (vector->list vec2)))
         (push-E! vec1))))
      ((multi)
       (cond
         ((= stack-len (+ imm 1))
          ;; Short cut
          (push-E! S))
         (else
           ;; Slow path
           (let* ((vec1 (make-vector (+ imm 1)))
                  (l (reverse (vector->list S)))
                  (resto (car l))
                  (args0 (reverse (cdr l))))
             (vm-args-decompose
               resto
               (lambda objs
                 (let ((args (append args0 objs)))
                  (unless (<= imm (length args))
                    (error "Argument count unmatch" S imm))
                  (let loop ((idx 0)
                             (cur args))
                    (cond
                      ((< idx imm)
                       (when (null? cur)
                         (error "Argument count unmatch??? (FIXME)"))
                       (vector-set! vec1 idx (car cur))
                       (loop (+ idx 1) (cdr cur)))
                      (else
                        (vector-set! vec1 imm
                                     (apply vm-args-compose cur)))))
                  (push-E! vec1))))))))
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
      ((call) (vector->list stack))
      ((multi)
       (let ((l (vector->list stack)))
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

  (define (call-primitive! type tail?)
    (let ((l (prepare-args type S)))
     (pop-S!)
     (call-with-values
       (lambda () (apply (vm-callable V) l))
       (lambda vals
         (case (length vals)
           ((0) (set! V #f)
                (set! link 'none))
           ((1) (set! V (car vals))
                (set! link 'single))
           (else (set! V (list->vector vals))
                 (set! link 'values)))
         ;; Perform synthetic RET on tail-call
         (when tail?
           (restore-dump!))))))
  (define (call-label! type)
    (set! link type)
    (apply-env! (vm-call-env V))
    (jump (vm-call-label V)))
  (define (save-dump!)
    (set! D* (cons (list S* E E* (vm-returnpoint)) D*)))
  (define (restore-dump!)
    (let ((a (car D*))
          (d (cdr D*)))
      (apply (lambda (ex-S* ex-E ex-E* returnpoint)
               (set! S '())
               (set! S* ex-S*)
               (pop-S!)
               (set! E ex-E)
               (set! E* ex-E*)
               (jump returnpoint))
             a)
      (set! D* d)))
  (define (call-dispatch! type tail?)
    (unless (eq? link 'single)
      (error "Invalid object for call" V))
    (let ((primitive? (vm-primitive? V)))
     (cond
       (primitive?
         (call-primitive! type tail?))
       (else
         (unless tail?
           (save-dump!))
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
       (set-value! (vector-ref E pos)))
      (else
        (let* ((offs (- frm 1))
               (vec (list-ref E* offs)))
          (set-value! (vector-ref vec pos))))))
  (define (LDG mod pos)
    (set-value! (global mod pos)))
  (define (LDF label)
    (set-value! (make-closure label (cons E E*))))
  (define (LDC cnr)
    (set-value! (constant cnr)))
  (define (LDV)
    (case (vector-length S)
      ((1) (set! V (vector-ref S 0))
           (set! link 'single))
      ((0) (set! V #f)
           (set! link 'none))
      (else
        (set! V S)
        (set! link 'values)))
    (pop-S!))
  (define (LDI imm)
    (set-value! imm))
  (define (LDN)
    (set-value! (make-unspecified)))
  (define (LDNN)
    (set! link 'none))

  (define (ST frm pos)
    (unless (eq? link 'single)
      (error "Invalid link status" link))
    (cond
      ((= frm 0)
       (vector-set! E pos V))
      (else
        (let* ((offs (- frm 1))
               (vec (list-ref E* offs)))
          (vector-set! vec pos V)))))
  (define (MOV loc)
    (unless (eq? link 'single)
      (error "Invalid link status" link))
    (vector-set! S loc V))
  
  ;; Machine cycle
  (define (cycle op arg0 arg1)
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
        (error "Invalid opcode" (list op arg0 arg1)))))


  ;; Extra query
  (define (extra op arg0 arg1)
    (case op
      ((RESULT)
       (case link
         ((single) V)
         ((values) (apply values (vector->list V)))
         ((none) (values))
         (else
           (error "No result available" link V))))
      (else
        (error "Invalid extra request" (list op arg0 arg1)))) )

  (values cycle extra))
         
)
