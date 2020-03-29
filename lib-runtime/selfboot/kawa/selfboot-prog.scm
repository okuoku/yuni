;;
;; Selfboot entrypoint for Kawa
;; 
;;  $ kawa /path/to/selfboot-entry.scm ....
;;

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

  (define (strip-extra-words cur l)
    (if (pair? l)
      (let ((a (car l))
            (d (cdr l)))
        (if (char=? #\space a)
          (list->string cur)
          (strip-extra-words (cons a cur) d)))
      (list->string cur)))

  (let ((r (pathcomponent '() '() pth)))
   (let ((first (strip-extra-words '() (reverse (string->list (car r))))))
     (pathcompose-start "" (start-simple '() first (cdr r))))))

(define (%%chop-first-arg lis)
  (let ((a (car lis))
        (d (cdr lis)))
    (let loop ((sl '())
               (q (string->list a)))
      (if (pair? q)
        (if (char=? #\space (car q))
          (append (list (list->string (reverse sl)) 
                        (list->string (cdr q)))
                  d)
          (loop (cons (car q) sl) (cdr q)))
        ;; Something wrong
        lis))))

(define %%selfboot-orig-command-line (%%chop-first-arg (command-line)))
(define %%selfboot-mypath (%%extract-entrypoint-path %%selfboot-orig-command-line))
(define %%selfboot-program-args (%%extract-program-args
                                  %%selfboot-orig-command-line
                                  %%selfboot-mypath))

(define (%%locate-yuniroot-fromscmpath scmpath)
  ;(write %%selfboot-orig-command-line) (newline)
  ;(write %%selfboot-mypath) (newline)
  (let ((npth (%%pathslashfy scmpath)))
   (%%pathsimplify (string-append npth "/../../../.."))))

(define %%selfboot-yuniroot (%%locate-yuniroot-fromscmpath %%selfboot-mypath))

(define (%%selfboot-loadlib pth libname imports exports)
  (let ((code (%selfboot-file->sexp-list pth)))
   ;(write (list 'LAUNCH: pth)) (newline)
   (eval `(define-library ,libname
                          (export ,@exports)
                          (import (yuni-runtime r7rs) ,@imports)
                          (begin ,@code)))))

(define (%%selfboot-load-program pth) (load pth))

(define (%%selfboot-load-aliaslib truename alias* export*)
  (for-each (lambda (libname)
              (let ((code `(define-library ,libname
                                    (export ,@export*)
                                    (import ,truename))))
                (eval code)))
            alias*))

(define %%selfboot-impl-type 'kawa)
(define %%selfboot-core-libs '((scheme base)
                               (scheme case-lambda)
                               (scheme cxr)
                               (scheme file)
                               (scheme inexact)
                               (scheme process-context)
                               (scheme read)
                               (scheme write)
                               (scheme eval)
                               (rnrs hashtables)
                               ))


(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

(load (string-append %%selfboot-yuniroot "/lib-runtime/r7rs/yuni-runtime/r7rs.sld"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/kawa/selfboot-runtime.scm"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/common.scm"))
(load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/run-program.scm"))

