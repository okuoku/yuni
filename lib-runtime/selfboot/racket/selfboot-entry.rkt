;;
;; Selfboot entrypoint for Racket (INCOMPLETE)
;; 
;;  $ racket /path/to/selfboot-entry.rkt ....
;;


#lang racket

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

(define myenv (make-base-namespace))
        
(define (xload pth) 
  (define (do-eval code)
    (for-each (lambda (e) 
                (write (list 'EVAL: e)) (newline)
                (eval e myenv))
              code))

  (write (list 'XLOAD: pth)) (newline)       
  (call-with-input-file
    pth
    (lambda (p)
      (let loop ((code '()))
       (let ((c (read p)))
        (if (eof-object? c)
          (do-eval (reverse code))
          (loop (cons c code))))))))

(define (load-libaliases truename alias* export*)
  (for-each (lambda (libname)
              (let ((code `(library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                (eval code myenv)))
            alias*))

(define (set-top-level-value! sym val env)
  ;; Chez scheme API
  ;; map? = #f, as-constant? = #f
  (namespace-set-variable-value! sym val #f env #f))
        
(define (launcher yuniroot program-args)
  (set-top-level-value! '%%selfboot-tmp-xload xload myenv)
  (set-top-level-value! '%%selfboot-yuniroot yuniroot myenv)
  (set-top-level-value! '%%selfboot-program-args program-args myenv)
  (set-top-level-value! '%%selfboot-impl-type 'racket myenv)
  (set-top-level-value! '%%selfboot-core-libs '((rnrs)
                                                (rnrs mutable-pairs)
                                                (rnrs mutable-strings)
                                                (rnrs r5rs))
                        myenv)
  (set-top-level-value! '%%selfboot-load-aliaslib load-libaliases myenv)
  (eval '(define load %%selfboot-tmp-xload) myenv)
  (xload (string-append yuniroot "/lib-runtime/selfboot/chez/selfboot-runtime.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/common.scm"))
  (xload (string-append yuniroot "/lib-runtime/selfboot/common/run-program.scm")))

(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))
                    
(launcher %%selfboot-yuniroot %%selfboot-program-args)
