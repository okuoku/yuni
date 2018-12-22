;;
;; Selfboot entrypoint for Guile
;; 
;;  $ guile --program /path/to/selfboot-entry.sps ....
;;


(import (guile) (rnrs))

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

  (let ((r (pathcomponent '() '() pth)))
   (pathcompose "" (simple '() (car r) (cdr r)))))


(define (%%locate-yuniroot-fromscmpath scmpath)
  (define MYNAME "selfboot-entry.scm")
  (write %%selfboot-orig-command-line) (newline)
  (write %%selfboot-mypath) (newline)
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

(define myenv (interaction-environment))

(define (file->sexp-list fn)
  (call-with-input-file 
    fn
    (lambda (p)
      (let loop ((cur '()))
       (let ((r (read p)))
        (if (eof-object? r)
          (reverse cur)
          (loop (cons r cur))))))))

(define (eval/filt code env)
  (define (filter-exports exports)
    (filter (lambda (e)
              (case e
                ((_ ... => else unquote unquote-splicing) #f)
                (else #t)))
            exports))
  (define (lib?)
    (and (pair? code)
         (eq? 'library (car code))))
  (if (lib?)
    (let ((exports (cdr (caddr code))))
     (eval `(library ,(cadr code) (export ,@(filter-exports exports))
                     ,@(cdddr code)) env))
    (eval code env)))
        
(define (xload pth) 
  (write (list 'XLOAD: pth)) (newline)       
  (let ((prog (file->sexp-list pth)))
   (for-each (lambda (e) 
               ;(write (list 'EVAL: e)) (newline)
               (eval/filt e myenv))
             prog)))

(define (load-libaliases truename alias* export*)
  (for-each (lambda (libname)
              (let ((code `(library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                ;(write (list 'ALIAS: truename '=> libname export*)) (newline)
                (eval/filt code myenv)))
            alias*))

(define (set-top-level-value! sym obj env)
  ;; Chez API
  (module-define! env sym obj))
        
(define (launcher yuniroot program-args)
  (set-top-level-value! '%%selfboot-tmp-xload xload myenv)
  (set-top-level-value! '%%selfboot-yuniroot yuniroot myenv)
  (set-top-level-value! '%%selfboot-program-args program-args myenv)
  (set-top-level-value! '%%selfboot-impl-type 'guile myenv)
  (set-top-level-value! '%%selfboot-core-libs '((rnrs)
                                                (rnrs mutable-pairs)
                                                (rnrs mutable-strings)
                                                (rnrs r5rs)
                                                (srfi :1)
                                                (srfi :9)
                                                (srfi :39)
                                                (srfi :98)
                                                (guile))
                        myenv)
  (set-top-level-value! '%%selfboot-load-aliaslib load-libaliases myenv)
  (eval '(define load %%selfboot-tmp-xload) myenv)
  (xload (string-append yuniroot "/lib-runtime/selfboot/guile/selfboot-runtime.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/common.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/run-program.scm")))
                    
(launcher %%selfboot-yuniroot %%selfboot-program-args)
