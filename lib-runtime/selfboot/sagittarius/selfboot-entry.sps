;;
;; Selfboot entrypoint for Sagittarius
;; 
;;  $ sagittarius /path/to/selfboot-entry.sps ....
;;


(import (scheme base)
        (scheme process-context)
        (scheme write)
        (scheme repl)
        (scheme file)
        (scheme eval)
        (scheme load))

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

(define (%%selfboot-loadlib pth libname imports exports)
  (load pth (interaction-environment)))


(define (%%selfboot-load-program pth)
  (load pth (interaction-environment)))

(define (%%selfboot-load-aliaslib truename alias* export*)
  (for-each (lambda (libname)
              (let ((code `(library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                (eval code (interaction-environment))))
            alias*))

(define %%selfboot-impl-type 'sagittarius)
(define %%selfboot-core-libs '((rnrs)
                               (rnrs base)
                               (rnrs control)
                               (rnrs hashtables)
                               (rnrs eval)
                               (rnrs mutable-pairs)
                               (rnrs mutable-strings)
                               (rnrs r5rs)
                               (scheme base)
                               (scheme case-lambda)
                               (scheme cxr)
                               (scheme file)
                               (scheme inexact)
                               (scheme process-context)
                               (scheme read)
                               (scheme write)
                               ))

(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))


(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/sagittarius/selfboot-runtime.scm"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/common.scm"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/run-program.scm")) 
        
