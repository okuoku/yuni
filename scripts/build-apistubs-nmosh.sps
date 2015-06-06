;; API stub build script for nmosh
;; Requires yuni. (run build-nmosh first, using run/buildstub.sh)

(import (yuni scheme)
        (yuni util files)
        (yuni ffi database stubir0)
        (yuni ffi ctemplate root)
        (yuni ffi database root)
        (yuni ffi database config)
        (yuni ffi database libinfo)
        (only (rnrs) fold-left))

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
  (path-append (fold-left (lambda (cur e)
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
(define APITOP "apidata")
(define apidefs '())

(define (output-path libname basename)
  (calc-filepath STUBTOP libname basename))

(define (procfile file)
  (define db (stubir0->database (car (file->sexp-list file))))
  (define config (database-config db))
  (define libinfo (database-libinfo db))
  (define c (config-stub-c config))
  (define cxx (config-stub-cxx config))
  (define (proc outfile)
    (call-with-output-file-force
      (output-path (libinfo-scheme-name libinfo) outfile)
      (lambda (p) (put-c-stubsource p db))))
  (when c (proc c))
  (when cxx (proc cxx)))

; Collect libraries
(define (collect-apidefs! dir)
  (define files '())
  (define (apidef? pth)
    (let ((e (path-extension pth)))
      (and e
           (or (string=? e "scm")))))
  ;; Recursively collect files on the dir
  (directory-walk dir (lambda (file)
                        (when (apidef? file)
                          (set! files (cons file files)))))
  (set! apidefs (append apidefs files)))

(collect-apidefs! APITOP)

(for-each procfile apidefs)
