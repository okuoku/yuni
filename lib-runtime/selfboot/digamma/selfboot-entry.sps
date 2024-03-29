;;
;; Selfboot entrypoint for Guile
;; 
;;  $ guile /path/to/selfboot-entry.sps ....
;;


(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scheme repl)
        (scheme eval)
        (scheme load)
        (rename 
          (only (core)
                ;; Digamma set-top-level-value! does not take environment
                set-top-level-value!  
                current-environment) 
          (set-top-level-value! set-top-level-value!/0)))

(define (%%extract-program-args args* entrypth)
  (if (string=? (car args*) entrypth)
    (cdr args*)
    (%%extract-program-args (cdr args*) entrypth)))

(define (%%extract-entrypoint-path args*)
  (define (checkone s)
    (and (string? s) 
         (let ((len (string-length s)))
          (and (< 4 len)
               (string=? (substring s (- len 4) len) ".sps")
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
      (let ((a (car q))
            (d (cdr q)))
        (if (string=? ".." a)
          (let ((next-cur (if (null? cur)
                            (list "..")
                            (cdr cur))))
            (if (null? d)
              (reverse next-cur)
              (simple next-cur (car d) (cdr d))))
          (simple (cons m cur) a d)))))
  (define (start-simple cur m q)
    ;; Protect relative ../../../ sequence at beginning
    (if (string=? m "..")
      (start-simple (cons m cur) (car q) (cdr q))
      (simple cur m q)))

  (let ((r (pathcomponent '() '() pth)))
   (pathcompose-start "" (start-simple '() (car r) (cdr r)))))

(define (%%locate-yuniroot-fromscmpath scmpath)
  (define MYNAME "selfboot-entry.scm")
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

(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

(define myenv (current-environment)) ;; FIXME: Don't use global env here...

(define (file->sexp-list fn)
  (call-with-input-file 
    fn
    (lambda (p)
      (let loop ((cur '()))
       (let ((r (read p)))
        (if (eof-object? r)
          (reverse cur)
          (loop (cons r cur))))))))
        
(define (xload pth) 
  ;(write (list 'XLOAD: pth)) (newline)       
  (let ((prog (file->sexp-list pth)))
   (for-each (lambda (e) 
               ;(write (list 'EVAL: e)) (newline)
               (eval e myenv))
             prog)))

(define (loadlib pth libname imports exports)
  (xload pth))

(define (load-libaliases truename alias* export*)
  (for-each (lambda (libname)
              (let ((code `(library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                ;(write (list 'ALIAS: truename '=> libname export*)) (newline)
                (eval code myenv)))
            alias*))

(define (set-top-level-value! sym val env)
  (parameterize ((current-environment env))
                (set-top-level-value!/0 sym val)))
        
(define (launcher yuniroot program-args)
  (set-top-level-value! '%%selfboot-tmp-xload xload myenv)
  (set-top-level-value! '%%selfboot-yuniroot yuniroot myenv)
  (set-top-level-value! '%%selfboot-program-args program-args myenv)
  (set-top-level-value! '%%selfboot-impl-type 'digamma myenv)
  (set-top-level-value! '%%selfboot-core-libs '((rnrs)
                                                (rnrs base)
                                                (rnrs control)
                                                (rnrs hashtables)
                                                (rnrs eval)
                                                (rnrs mutable-pairs)
                                                (rnrs mutable-strings)
                                                (rnrs r5rs)
                                                (srfi :1)
                                                (srfi :9)
                                                (srfi :39)
                                                (srfi :98)
                                                (core)
                                                (core files)
                                                (scheme base)
                                                (scheme case-lambda)
                                                (scheme cxr)
                                                (scheme file)
                                                (scheme inexact)
                                                (scheme process-context)
                                                (scheme eval)
                                                (scheme read)
                                                (scheme write))
                        myenv)
  (set-top-level-value! '%%selfboot-load-aliaslib load-libaliases myenv)
  (set-top-level-value! '%%selfboot-loadlib loadlib myenv)
  (eval '(define load %%selfboot-tmp-xload) myenv)
  (eval '(define %%selfboot-load-program %%selfboot-tmp-xload) myenv)
  (xload (string-append yuniroot "/lib-runtime/selfboot/digamma/selfboot-runtime.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/common.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/run-program.scm")))
                    
(launcher %%selfboot-yuniroot %%selfboot-program-args)
