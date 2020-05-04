(import ;(scheme base)
        (scheme process-context)
        (scheme read)
        ;(scheme cxr)
        (scheme write)
        ;(scheme repl)
        (scheme file)
        (scheme eval)
        ;(scheme load)
        (scheme inexact)
        )

(define (%%extract-program-args args* entrypth)
  (if (string=? (car args*) entrypth)
    (cdr args*)
    (%%extract-program-args (cdr args*) entrypth)))

(define (%%extract-entrypoint-path args*)
  (define (checkone s)
    (and (string? s) 
         (let ((len (string-length s)))
          (and (< 4 len)
               (string=? (substring s (- len 4) len) ".scm")
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
    (write (list 'simple cur m q)) (newline)
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

(define myenv (interaction-environment))

(define (%%selfboot-loadlib pth libname imports exports)
  #|
  (eval `(define-library ,libname
                         (export ,@exports)
                         (import (yuni-runtime r7rs) ,@imports)
                         (include ,pth)))
  |#
  ;(write (list 'LOADLIB pth)) (newline)
  (load pth myenv))

(define (%%selfboot-load-program pth) 
  (define (readprog fn)
    (call-with-input-file
      fn
      (lambda (p)
        (let loop ((cur '()))
         (let ((r (read p)))
          (if (eof-object? r)
            (reverse cur)
            (loop (cons r cur))))))))
  (write (list 'LOADPROG pth)) (newline)
  (let ((sexp (readprog pth)))
   (unless (and (pair? sexp) (pair? (car sexp))
                (eq? 'import (caar sexp)))
     (error "Program must start with (import ...)"))

   (for-each
     (lambda (exp)
       (write (list 'eval: exp)) (newline)
       (eval exp myenv))
     (cdr sexp))))


(define (%%selfboot-load-aliaslib truename alias* export*)
  #|
  (for-each (lambda (libname)
              (let ((code `(define-library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                (eval code)))
            alias*)
  |#
  'do-nothing)

(define %%selfboot-impl-type 'cyclone)
(define %%selfboot-core-libs '((scheme base)
                               (scheme case-lambda)
                               (scheme cxr)
                               (scheme file)
                               (scheme inexact)
                               (scheme process-context)
                               (scheme read)
                               (scheme write)
                               (scheme eval)
                               (srfi 69)
                               ))


(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

;; Setup environment
(define (inject-var! sym obj)
  ((eval `(begin (define ,sym #f) (lambda (obj) (set! ,sym obj))) myenv)
   obj))

(set! %%selfboot-program-args (list "vectors0.sps")) ;; DEBUG DEBUG
(inject-var! '%%selfboot-yuniroot %%selfboot-yuniroot)
(inject-var! '%%selfboot-program-args %%selfboot-program-args)
(inject-var! '%%selfboot-impl-type %%selfboot-impl-type)
(inject-var! '%%selfboot-core-libs %%selfboot-core-libs)
(inject-var! '%%selfboot-loadlib %%selfboot-loadlib)
(inject-var! '%%selfboot-load-aliaslib %%selfboot-load-aliaslib)
(inject-var! '%%selfboot-load-program %%selfboot-load-program)
(inject-var! '%%myenv myenv)

;(load (string-append %%selfboot-yuniroot "/lib-runtime/r7rs/yuni-runtime/r7rs.sld"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/cyclone/selfboot-runtime.scm") myenv)
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/common.scm") myenv)
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/run-program.scm") myenv) 
