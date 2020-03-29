;;
;; Selfboot entrypoint for Racket
;; 
;;  $ racket /path/to/selfboot-entry.rkt ....
;;


#lang racket

(require (prefix-in r6rs: r6rs/lang/reader))

(define (command-line) (cons 
                         (path->string 
                           (path->complete-path (find-system-path 'run-file)))
                         (vector->list (current-command-line-arguments))))

(define (%%extract-program-args args* entrypth)
  (if (string=? (car args*) entrypth)
    (cdr args*)
    (%%extract-program-args (cdr args*) entrypth)))

(define (%%extract-entrypoint-path args*)
  (define (checkone s)
    (and (string? s) 
         (let ((len (string-length s)))
          (and (< 4 len)
               (string=? (substring s (- len 4) len) ".rkt")
               s))))
  (and (pair? args*)
       (or (checkone (car args*))
           (%%extract-entrypoint-path (cdr args*)))))

(define (%%pathslashfy pth)
  (let* ((l (string->list pth))
         (x (map (lambda (c) (if (char=? #\\ c) #\/ c)) l)))
    (list->string x)))

(define (%%pathsimplify pth)
  (define (pathcompose acc l)
    (if (pair? l)
      (pathcompose (if (string=? (car l) "")
                     acc
                     (string-append acc "/" (car l)))
                   (cdr l))
      acc))
  (define (pathcompose-start acc l)
    (pathcompose (car l) (cdr l)))
  (define (pathcomponent acc cur strq)
    (if (string=? strq "")
      (if (null? acc)
        (reverse cur)
        (reverse (cons (list->string (reverse acc)) cur)))
      (let ((c (string-ref strq 0))
            (r (substring strq 1 (string-length strq))))
        (if (char=? c #\/)
          (pathcomponent '() (cons (list->string (reverse acc)) cur) r)
          (pathcomponent (cons c acc) cur r)))))
  (define (simple cur m q)
    (if (null? q)
      (if (null? cur)
        (reverse (cons m cur))
        (reverse (cdr cur)))
      (if (string=? m "..")
        (simple (cdr cur) (car q) (cdr q))  
        (simple (cons m cur) (car q) (cdr q)))))
  (define (start-simple cur m q)
    ;; Protect relative ../../../ sequence at beginning
    (if (string=? m "..")
      (start-simple (cons m cur) (car q) (cdr q))
      (simple cur m q)))

  (let ((r (pathcomponent '() '() pth)))
   (pathcompose-start "" (start-simple '() (car r) (cdr r)))))

(define (%%locate-yuniroot-fromscmpath scmpath)
  ;(write %%selfboot-orig-command-line) (newline)
  ;(write %%selfboot-mypath) (newline)
  (let ((npth (%%pathslashfy scmpath)))
   (%%pathsimplify (string-append npth "/../../../.."))))

(define %%selfboot-orig-command-line (command-line))
(define %%selfboot-mypath (%%extract-entrypoint-path %%selfboot-orig-command-line))
(define %%selfboot-yuniroot (%%locate-yuniroot-fromscmpath %%selfboot-mypath))
(define %%selfboot-program-args (%%extract-program-args
                                  %%selfboot-orig-command-line
                                  %%selfboot-mypath))

(define myenv (make-base-namespace))

(define-namespace-anchor %%yuni-selfboot)

(define (xload pth) 
  (define import-done? #f)
  (define (do-eval code)
    (for-each (lambda (e) 
                (cond
                  ((and (not import-done?) (pair? e) (eq? 'import (car e)))
                   (let ((reqclause* (map convert-import (cdr e))))
                    (eval-with-local-resolver `(local-require ,@reqclause*)))
                   (set! import-done? #t))
                  (else
                    ;(write (list 'EVAL: e)) (newline)
                    (eval e myenv))))
              code))

  ;(write (list 'XLOAD: pth)) (newline)       
  (call-with-input-file
    pth
    (lambda (p)
      (let loop ((code '()))
       (let ((c (read p)))
        (if (eof-object? c)
          (do-eval (reverse code))
          (loop (cons c code))))))))

(define (read-r6rs-source pth)
  (call-with-input-file
    pth
    (lambda (p)
      (r6rs:read-syntax (object-name p) p #'dummy 1 0 1))))

(define h-libnames (make-hash))
(define h-libsyms (make-hash))

(define (eval-with-local-resolver frm)
  (let ((rr (current-module-name-resolver)))
   (define res
     (case-lambda
       ((r0 r1) 'ignored)
       ((mod src-name relative? unknown)
        (let* ((q (hash-ref h-libsyms mod #f)))
         ;(write (list 'RESOLV: mod '=> q)) (newline)
         (if q
           (make-resolved-module-path mod)
           (rr mod src-name relative? unknown))))))
   (parameterize ((current-module-name-resolver res))
                 (eval frm myenv))))

(define (libname->symbol name)
  (let loop ((cur "")
             (q name))
   (if (null? q)
     (string->symbol cur)
     (loop (string-append cur 
                          (if (string=? "" cur) "" "_")
                          (symbol->string (car q))) 
           (cdr q)))))

(define (mlist->list x) (for/list ((y (in-mlist x))) y))

(define (load-libaliases truename alias* export*)
  (let ((x (hash-ref h-libnames (mlist->list truename))))
   (for-each (lambda (libname)
               ;(write (list 'ALIAS x truename '=> libname)) (newline)
               (hash-set! h-libnames (mlist->list libname) x))
             (mlist->list alias*))))

(define (dumpsy x)
  (for-each
    (lambda (e)
      (cond
        ((list? (syntax->datum e))
         (dumpsy e))
        (else
          (write e) (newline) )))
    (syntax->list x)))

(define (symbolic-identifier=? x y) ;; From TSPL
  (eq? (syntax->datum x)
       (syntax->datum y)))

(define (convert-import clause)
  (let ((head (car clause)))
   (case head
     ((only) `(only-in ,(convert-import (cadr clause)) . ,(cddr clause)))
     ((except) `(except-in ,(convert-import (cadr clause)) . ,(cddr clause)))
     ((rename) `(rename-in ,(convert-import (cadr clause)) . ,(cddr clause)))
     ;; FIXME: Implement multiple meta-levels
     ((for) 
      (unless (equal? '(run expand) (cddr clause))
        (error 'xxx "Unknown phase level" clause))
      `(for-syntax ,(convert-import (cadr clause))))
     (else
       (hash-ref h-libnames clause)))))

(define (loadlib pth libname0 imports exports)
  (let* ((libname (mlist->list libname0))
         (libsym (libname->symbol libname)))
    (hash-set! h-libnames libname libsym)
    (hash-set! h-libsyms libsym #t)
    ;(write (list 'LOAD libname '=> libsym)) (newline)
    (let ((x (read-r6rs-source pth)))
     (syntax-case* 
       x (library export import) symbolic-identifier=?
       ((_ xxx dummy (module-begin (library libname 
                                           (export exports ...)
                                           (import imports ...)
                                           body ...)))
        (let ((x-imports (syntax->datum #'(imports ...)))
              (x-exports (syntax->datum #'(exports ...)))
              (x-body (syntax->datum #'(body ...))))

          (let ((code `(module ignored racket/base
                               (require ,@(map convert-import x-imports))
                               (provide ,@x-exports)
                               ,@x-body)))
            (parameterize 
              ((current-namespace myenv)
               (current-module-declare-name (make-resolved-module-path libsym)))
              (eval-with-local-resolver code)))))))))


(define (set-top-level-value! sym val env)
  ;; Chez scheme API
  ;; map? = #f, as-constant? = #f
  (namespace-set-variable-value! sym val #f env #f))
        
(define (launcher yuniroot program-args)
  (define (eval/yuni code)
    (define (conv p)
      (cond
        ((mpair? p) (cons (conv (mcar p))
                          (conv (mcdr p))))
        ((vector? p) (list->vector
                       (map conv (vector->list p))))
        (else p)))
    (eval (conv code) myenv))
  (parameterize ((current-namespace myenv))
                (namespace-require 'rnrs)
                (namespace-require 'rnrs/mutable-pairs-6))
  (set-top-level-value! '%%selfboot-tmp-xload xload myenv)
  (set-top-level-value! '%%selfboot-yuniroot yuniroot myenv)
  ;; Define program-args as mpair
  (eval `(define %%selfboot-program-args (quote ,program-args)) myenv)
  ;; ... and same for core-libs
  (eval '(define %%selfboot-core-libs (quote ((rnrs)
                                              (rnrs base)
                                              (rnrs control)
                                              (rnrs hashtables)
                                              (rnrs eval)
                                              (rnrs mutable-pairs)
                                              (rnrs mutable-strings)
                                              (rnrs r5rs)
                                              (racket base)
                                              (srfi :1)
                                              (srfi :9)
                                              (srfi :39)
                                              (srfi :98)
                                              (yuniselfboot-internals)
                                              )))
        myenv)
  (for-each (lambda (e)
              (let ((libname (car e))
                    (libsym (cdr e)))
                (hash-set! h-libnames libname libsym)))
            '(((rnrs) . rnrs)
              ((rnrs base) . rnrs/base-6)
              ((rnrs control) . rnrs/control-6)
              ((rnrs hashtables) . rnrs/hashtables-6)
              ((rnrs eval) . rnrs/eval-6)
              ((rnrs mutable-pairs) . rnrs/mutable-pairs-6)
              ((rnrs mutable-strings) . rnrs/mutable-strings-6)
              ((rnrs r5rs) . rnrs/r5rs-6)
              ((racket base) . racket/base)
              ((srfi :1) . srfi/%3a1)
              ((srfi :9) . srfi/%3a9)
              ((srfi :39) . srfi/%3a39)
              ((srfi :98) . srfi/%3a98)
              ((yuniselfboot-internals) . yuniselfboot-internals)))
  (set-top-level-value! '%%selfboot-impl-type 'racket myenv)
  (set-top-level-value! '%%selfboot-load-aliaslib load-libaliases myenv)
  (set-top-level-value! '%%selfboot-loadlib loadlib myenv)
  (set-top-level-value! 'eval/yuni eval/yuni myenv)
  (eval '(define load %%selfboot-tmp-xload) myenv)
  (eval '(define %%selfboot-load-program %%selfboot-tmp-xload) myenv)
  (xload (string-append yuniroot "/lib-runtime/selfboot/racket/selfboot-runtime.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/common.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/run-program.scm")))
        
(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))
                    
(launcher %%selfboot-yuniroot %%selfboot-program-args)
