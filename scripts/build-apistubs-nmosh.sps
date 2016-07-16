;; API stub build script for yuni
;; Requires yuni. (run build-nmosh first, using run/buildstub.sh)

(import (yuni scheme)
        (yuni util files)
        (yuni ffi database stubir0)
        (yuni ffi ctemplate root)
        (yuni ffi scmtemplate root)
        (yuni ffi database root)
        (yuni ffi database config)
        (yuni ffi database libinfo))

(define (fold-left1 proc init lis)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (let ((c (proc cur a)))
         (itr c d)))
      cur))
  (itr init lis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utilities
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (call-with-output-file-force file proc)
  (define (mkdirp cur)
    (define dir (path-dirname cur))
    (unless (string=? "" dir)
      (unless (file-exists? dir)
        (mkdirp dir)
        (create-directory dir))))
  (when (file-exists? file)
    (delete-file file))
  (mkdirp file)
  (write (list 'GENERATING: file))(newline)
  (call-with-output-file file proc))

(define (calc-filepath basepath sexp fname)
  (path-append (fold-left1 (lambda (cur e)
                            (path-append cur (symbol->string e)))
                          basepath
                          sexp)
               fname))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STUBTOP "lib-stub/apistubs")
(define STUBTOP/SCM "lib-stub/gen")
(define APITOP* '("apidata" "tests/yunistub"))
(define apidefs '())

(define (output-path libname basename)
  (calc-filepath STUBTOP libname basename))

(define (output-path/scm libname)
  (define r (reverse libname))
  (define basename (string-append (symbol->string (car r)) ".sls"))
  (define rest (reverse (cdr r)))
  (calc-filepath STUBTOP/SCM rest basename))

(define (stubir0? pth)
  (define fil (car (file->sexp-list pth)))
  (and (pair? fil)
       (eq? 'stubir0 (car fil))))

(define (procfile file)
  (define db (stubir0->database (car (file->sexp-list file))))
  (define config (database-config db))
  (define libinfo (database-libinfo db))
  (define c (and config (config-stub-c config)))
  (define cxx (and config (config-stub-cxx config)))
  (define (proc outfile)
    (call-with-output-file-force
      (output-path (libinfo-scheme-name libinfo) outfile)
      (lambda (p) (put-c-stubsource p db))))
  ;; emit C apistubs
  (when c (proc c))
  (when cxx (proc cxx))
  ;; emit scm stub
  (when (or c cxx) 
    (call-with-output-file-force
      (output-path/scm (get-scm-libname/constants db))
      (lambda (p) (put-scm-stubsource/constants p db)))))

; Collect libraries
(define (collect-apidefs! dir)
  (define files '())
  (define (apidef? pth)
    (let ((e (path-extension pth)))
      (and e
           (or (string=? e "scm"))
           (stubir0? pth))))
  ;; Recursively collect files on the dir
  (directory-walk dir (lambda (file)
                        (when (apidef? file)
                          (set! files (cons file files)))))
  (set! apidefs (append apidefs files)))

(for-each collect-apidefs! APITOP*)

(for-each procfile apidefs)
