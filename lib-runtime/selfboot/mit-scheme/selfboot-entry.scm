;;
;; Selfboot entrypoint for MIT/GNU Scheme
;; 
;;  $ mit-scheme /path/to/selfboot-entry.scm ....
;;

(param:reader-fold-case? #f) ;; Requires 10.1.5

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
  (write %%selfboot-orig-command-line) (newline)
  (write %%selfboot-mypath) (newline)
  (let ((npth (%%pathslashfy scmpath)))
   (%%pathsimplify (string-append npth "/../../../.."))))

(define (%selfboot-file->sexp-list fn)
  (call-with-input-file
    fn
    (lambda (p)
      (let loop ((cur '()))
       (let ((r (read p)))
        (if (eof-object? r)
          (reverse cur)
          (loop (cons r cur))))))))

(define %selfboot-file-exists? file-exists?)

(define (%selfboot-load prefix files)
  (for-each (lambda (e)
              (load (string-append %%selfboot-yuniroot "/"
                                   prefix "/" e)))
            files))

(define %%selfboot-orig-command-line (command-line))
(define %%selfboot-mypath (%%extract-entrypoint-path %%selfboot-orig-command-line))
(define %%selfboot-yuniroot (%%locate-yuniroot-fromscmpath %%selfboot-mypath))
(define %%selfboot-program-args (%%extract-program-args
                                  %%selfboot-orig-command-line
                                  %%selfboot-mypath))

(define myenv (interaction-environment))

(define %%selfboot-impl-type 'mit-scheme)
(define %%selfboot-core-libs '((scheme base)
                               (scheme case-lambda)
                               (scheme cxr)
                               (scheme file)
                               (scheme inexact)
                               (scheme process-context)
                               (scheme read)
                               (scheme write)
                               (scheme eval)
                               (scheme load)
                               ))

(define (%%selfboot-error-hook c)
  (display "ERROR!\n" (trace-output-port))
  (write-condition-report c (trace-output-port))
  (newline (trace-output-port))
  (exit 1))


(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

;(environment-assign! myenv '%selfboot-load %selfboot-load)

(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define (command-line) %%selfboot-current-command-line)

(define %%selfboot-mainprog #f)

(define %%selfboot-yuni-scheme-replacements
  '((scheme base)
    (scheme case-lambda)
    (scheme cxr)
    (scheme file)
    (scheme inexact)
    (scheme process-context)
    (scheme read)
    (scheme write))) 

(define (%%selfboot-filter-imports lis)
  (let loop ((cur '())
             (q lis))
    (if (not (pair? q))
      (reverse cur)
      (let ((lib (car q))
            (rest (cdr q)))
        (cond
          ((equal? lib '(yuni scheme))
           (loop (append %%selfboot-yuni-scheme-replacements cur)
                 rest))
          (else (cons lib cur)))))))

(define (%%selfboot-runcode temppath)
  (define loadprog #f)
  (set! temppath "out.scm")
  ;; Scan arguments
  (let loop ((q %%selfboot-current-command-line))
   (if (pair? q)
     (let ((a (car q))
           (d (cdr q)))
       (cond
         ((string=? "-LIBPATH" a)
          (let ((dir (car d)))
           (set! %%selfboot-current-libpath
             (cons dir
                   %%selfboot-current-libpath))
           (loop (cdr d))))
         (else
           (set! %%selfboot-current-command-line q))))
     #t))

  ;; Generate stub prog
  (write (list 'Stubpath: temppath)) (newline)
  (call-with-output-file
    temppath
    (lambda (p)
      ;; Collect deps and load
      (let ((r (command-line)))
       (let* ((prog (car r))
              (codetab (%%selfboot-gen-filelist
                         %%selfboot-current-libpath
                         (list prog))))
         (for-each (lambda (e)
                     (let ((dir (cadr e))
                           (pth (caddr e))
                           (truename (car e))
                           (aliasnames (cadddr e))
                           (raw-imports (car (car (cddddr e))))
                           (exports (cdr (car (cddddr e)))))
                       (let* ((filepth (string-append dir "/" pth))
                              (raw-libcode (car (%selfboot-file->sexp-list
                                                  filepth)))
                              (imports* (%%selfboot-filter-imports
                                          raw-imports)))
                        (pp `(define-library ,truename
                                                (export ,@exports)
                                                (import 
                                                  ,@imports*)
                                                (begin ,@(cddddr raw-libcode)))
                               p)
                        (newline p)
                        (when aliasnames
                          (for-each (lambda (nam)
                                      (pp `(define-library 
                                                ,nam
                                                (export ,@exports)
                                                (import ,truename))
                                               p)
                                      (newline p))
                                    aliasnames))
                        (write (list 'LOAD: filepth)) (newline))))
                   codetab)

         (let ((progbody (%selfboot-file->sexp-list prog)))
           (for-each (lambda (e) (pp e p))
                     progbody))))))
  (load temppath))

(fluid-let ;; FIXME: Use parameterize
  ((standard-error-hook %%selfboot-error-hook))
  (load (string-append %%selfboot-yuniroot "/lib-runtime/r7rs/yuni-runtime/r7rs.sld"))
  (load (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/common.scm"))
  (call-with-temporary-file-pathname %%selfboot-runcode))

(exit 0)
