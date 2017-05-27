(library (yunivm compiler compilercore)
         (export compile-core)
         (import (yuni scheme))

         
;;

;; Compiler supports following syntaxes
;; 
;; let let* letrec letrec* --- no named-lambda support
;; if
;; when
;; lambda
;; begin
;; define -------------------- special handling for global-define
;; set!
;; (standard procedure call)

(define (compile-core scm vec-global0) ;; => treeir max-block
  (define max-blockid 0)
  (define (new-blockid!)
    (let* ((cur max-blockid)
           (next (+ 1 cur)))
      (set! max-blockid next)
      cur)) 
  ;; Env lookup
  (define (sym->pos frmveclis sym) ;; => #f / integer
    (define (itr-vec len pos)
      (and (not (= len pos))
           (or (and (eq? (vector-ref frmveclis pos) sym) pos)
               (itr-vec len (+ 1 pos)))))
    (define (itr-lis pos cur)
      (and (pair? cur)
           (let ((a (car cur))
                 (d (cdr cur)))
             (or (and (eq? sym a) pos)
                 (itr-lis (+ pos 1) d)))))
    (cond
      ((vector? frmveclis)
       (itr-vec (vector-length frmveclis) 0))
      ((pair? frmveclis)
       (itr-lis 0 frmveclis))
      ((null? frmveclis)
       #f)
      (else
        (error "Invalid env object" frmveclis))))

  (define (sym->frmpos env sym) ;; => #f #f / int int
    (define (itr frmpos rest)
      (cond
        ((pair? rest)
         (let ((frm (car rest))
               (next (cdr rest)))
           (let ((pos? (sym->pos frm sym)))
            (if pos?
              (values frmpos pos?)
              (itr (+ 1 frmpos) next)))))
        (else (values #f #f))))
    (itr 0 env))
  
  (define (lambda-args->type+frm frm) ;; => multi | call
    (cond
      ((symbol? frm)
       (values 'multi (list frm)))
      ((list? frm)
       (values 'call frm))
      ((pair? frm)
       (let loop ((cur '())
                  (rest frm))
        (if (pair? rest)
          (loop (cons (car rest) cur) (cdr rest))
          (begin
            (unless (symbol? rest)
              (error "Invalid lambda frm" frm))
            (values 'multi (reverse (cons rest cur)))))))
      (else
        (error "Unknown object for lambda frm" frm))))

  ;; Instruction gen
  (define (sym->gen-inst env sym k)
    (call-with-values (lambda () (sym->frmpos env sym))
                      k)) 
  ;;; MOV
  (define (pos->moveinst pos)
    (unless (integer? pos)
      (error "Invalid argument for MOV inst" pos))
    (list 'MOV pos))

  ;;; ST
  (define (sym->set!inst env sym)
    (sym->gen-inst env sym 
                   (lambda (frm pos)
                     (unless (and frm pos)
                       (error "Unknown symbol for set!" sym))
                     (list 'ST frm pos))))

  ;;; LDI LD LDG
  (define (obj->loadinst env obj) ;; => inst
    (cond
      ((or (number? obj)
           (string? obj)
           (char? obj)
           (null? obj)
           (boolean? obj))
       ;; Immediate
       (list 'LDI obj))
      ((symbol? obj)
       (sym->gen-inst env obj
                      (lambda (frm pos)
                        (cond
                          ((and frm pos)
                           (list 'LD frm pos))
                          (else ;; second chance for global object
                            (let ((pos (sym->pos vec-global0 obj)))
                             (unless pos
                               (error "unknown symbol for fetch" obj))
                             (list 'LDG 0 pos)))))))
      (else
        (error "Invalid object for loadinst" obj))))
  
  ;; Standard sequences
  (define (compile-set! env seq k)
    (unless (and (list? seq) (= 3 (length seq)))
      (error "Invalid list for set!" seq))
    (let ((target (cadr seq))
          (val (caddr seq)))
      (compile-form
        #f env val
        (lambda ()
          (cons (sym->set!inst env target)
                (k))))))

  (define (compile-if3 tail? env seq k)
    (unless (and (list? seq) (= 4 (length seq)))
      (error "Invalid list for if" seq))
    (let ((blkno0 (new-blockid!))
          (blkno1 (new-blockid!))
          (check (cadr seq))
          (then-body (caddr seq))
          (else-body (cadddr seq)))
      (cons 
        (cons 
          'block 
          (cons blkno0
                (compile-form
                  #f 
                  env check
                  (lambda ()
                    (cons (list 'BRV (list 'enter blkno1))
                          (compile-form
                            tail?
                            env else-body
                            (lambda ()
                              (cons (list 'JMP (list 'break blkno0))
                                    (list 
                                      (cons 'block 
                                            (cons 
                                              blkno1
                                              (compile-form
                                                tail?
                                                env then-body
                                                (lambda () '())))))))))))))
        (k))))

  (define (compile-if2 tail? env seq k)
    (unless (and (list? seq) (= 3 (length seq)))
      (error "Invalid list for when/if2" seq))
    (let ((blkno0 (new-blockid!))
          (blkno1 (new-blockid!))
          (check (cadr seq))
          (body (caddr seq)))
      (cons 
        (cons 
          'block 
          (cons blkno0
                (compile-form
                  #f 
                  env check
                  (lambda ()
                    (cons (list 'BRV (list 'enter blkno1))
                          (cons (list 'LDN)
                                (cons (list 'JMP (list 'break blkno0))
                                      (list
                                        (cons 'block
                                              (cons 
                                                blkno1
                                                (compile-form
                                                  tail?
                                                  env body
                                                  (lambda () '()))))))))))))
            (k))))

  (define (compile-procedure-call tail? env seq k)
    (unless (list? seq)
      (error "Invalid list for procedure-call" seq))
    (let* ((len (length seq))
           (argcount (- len 1))
           (proc (car seq))
           (args (cdr seq))
           (callinst (if tail? 'TCALL 'CALL)))
      (define (call-proc)
        ;; Never be a tail 
        (compile-form #f env proc 
                      (lambda ()
                        (cons (list callinst)
                              (k)))))
      (define (load-args num args)
        (cond
          ((= num argcount)
           (call-proc))
          (else
            (let ((var (car args))
                  (next (cdr args))
                  (nextnum (+ 1 num)))
              (compile-form #f env var
                            (lambda ()
                              (cons (list 'MOV num)
                                    (load-args nextnum next))))))))
      (case argcount
        ((0) (cons (list 'FRAME argcount)
                   (call-proc)))
        (else (cons (list 'FRAME argcount)
                    (load-args 0 args))))))

  (define (compile-let tail? env seq k)
    ;; NB: Assume alpha-transform
    (let* ((vars (cadr seq))
           (varcount (length vars))
           (varnames (map car vars))
           (bodyseq (cddr seq))
           (current-env (cons varnames env)))
      (define (set-vars v*)
        (cond 
          ((null? v*)
           (compile-lambda-body tail? current-env bodyseq
                                (lambda ()
                                  (cons (list 'LEAVE)
                                        (k)))))
          (else
            (let* ((vv (car v*))
                   (varname (car vv))
                   (varbody (cadr vv)))
              (unless (and (list? vv) (= (length vv) 2))
                (error "Malformed let" vv))
              ;(display (list 'LETVAR: varname 'FRM: varbody)) (newline)
              (compile-form #f current-env varbody
                            (lambda ()
                              (cons (sym->set!inst current-env varname)
                                    (set-vars (cdr v*)))))))))
      (cons (list 'FRAME varcount)
            (cons (list 'BIND)
                  (set-vars vars)))))

  (define (compile-lambda env seq k)
    (unless (and (list? seq) (>= (length seq) 3))
      (error "Malformed lambda" seq))
    (let* ((frms (cadr seq))
           (body (cddr seq))
           (blkno0 (new-blockid!))
           (blkno1 (new-blockid!)))
      (define (lambda-recv type len)
        (case type
          ((multi) (list 'RECVM (- len 1)))
          (else (list 'RECV len))))
      (define (lambda-body current-env body)
        (compile-lambda-body #t ;; Every lambda-body has a tail form
                             current-env body
                             (lambda () (list (list 'RET)))))
      (define (lambda-emit type frm)
        (let ((recv (lambda-recv type (length frm))))
         (cons recv
               (lambda-body (cons frm env) body))))
      (define (lambda-main)
        (call-with-values (lambda () (lambda-args->type+frm frms))
                          lambda-emit))
      ;(display (list 'LAMBDA: seq)) (newline)
      (cons (list 'block blkno0
                  (list 'LDF (list 'enter blkno1))
                  (list 'JMP (list 'break blkno0))
                  (cons 'block 
                        (cons  blkno1
                               (lambda-main))))
            (k))))

  ;;; 
  ;;
  (define (compile-form tail? env frm k)
    (cond
      ((pair? frm)
       ;(display (list 'FRM: frm)) (newline)
       (case (car frm)
         ((quote)
          (cons (list 'LDI (cadr frm))
                (k)))
         ((lambda) (compile-lambda env frm k))
         ((let let* letrec letrec*)
          (compile-let tail? env frm k))
         ((set!)
          (compile-set! env frm k))
         ((if)
          (let ((len (and (list? frm) (length frm))))
           (case len
             ((3) (compile-if2 tail? env frm k))
             ((4) (compile-if3 tail? env frm k))
             (else
               (error "Malformed if" frm)))))
         ((when)
          (compile-if2 tail? env frm k))
         ((begin)
          (compile-sequence tail? env (cdr frm) k))
         ((define global-define)
          (error "??" frm))
         (else (compile-procedure-call tail? env frm k))))
      (else (cons (obj->loadinst env frm)
                  (k)))))
  (define (compile-sequence tail? env seq k)
    (cond
      ((pair? seq)
       ;; Assume define was stripped macro-expander
       (let ((frm (car seq))
             (next (cdr seq)))
         (cond
           ((null? next)
            ;; Tail form
            (compile-form tail? env frm k))
           (else
             ;; Non-tail form
             (compile-form #f env frm
                           (lambda ()
                             (compile-sequence tail? env next k)))))) )
      (else (compile-form #f env seq k))))

  (define (compile-lambda-body tail? env seq k)
    (define (gen-letrec* defs seq)
      (cons 'letrec*
            (cons (map (lambda (e)
                         (let ((nam? (car e)))
                          (cond
                            ((symbol? nam?) e)
                            ((pair? nam?)
                             (let ((nam (car nam?))
                                   (args (cdr nam?)))
                               (list nam
                                     (cons 'lambda
                                           (cons args
                                                 (cdr e)))))))))
                       defs)
                  seq)))
    (define (exit defs seq)
      (if (null? defs)
        (compile-sequence tail? env seq k)
        (compile-form tail? env (gen-letrec* (reverse defs) seq) k)))
    (define (scan defs seq)
      (cond
        ((pair? seq)
         (let ((frm (car seq))
               (next (cdr seq)))
           (cond
             ((pair? frm)
              (case (car frm)
                ((define global-define)
                 (scan (cons (cdr frm) defs) next))
                (else
                  (exit defs seq))))
             (else (exit defs seq)))))
        (else (exit defs seq))))
    (scan '() seq))

  (let ((out (compile-lambda-body #f '() scm
                                  (lambda () '()))))
    (values out max-blockid)))
)
