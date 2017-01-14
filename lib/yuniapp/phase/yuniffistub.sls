(library (yuniapp phase yuniffistub)
         (export yuniffistub-app-cmd)
         (import (yuni scheme)
                 (yuni ffi database stubir0)
                 (yuni ffi ctemplate root)
                 (yuni ffi scmtemplate root)
                 (yuni ffi database root)
                 (yuni ffi database config)
                 (yuni ffi database libinfo))

;; (yuni util files) excerpt
(define (file->list proc pth)
  (with-input-from-file
    pth
    (lambda ()
      (define (itr cur)
        (let ((r (proc (current-input-port))))
          (if (eof-object? r)
            (reverse cur)
            (itr (cons r cur)))))
      (itr '()))))

(define (file->sexp-list pth)
  (file->list read pth))


(define (call-with-output-file-force file proc)
  (when (file-exists? file)
    (delete-file file))
  (call-with-output-file file proc))

(define (procfile file scmdir cdir)
  (define (output-path/scm libname)
    (define r (car (reverse libname)))
    (define basename (string-append (symbol->string r) ".sls"))
    basename)
  (define db (stubir0->database (car (file->sexp-list file))))
  (define config (database-config db))
  (define libinfo (database-libinfo db))
  (define c (and config (config-stub-c config)))
  (define cxx (and config (config-stub-cxx config)))
  (define (proc outfile)
    (call-with-output-file-force
      (string-append cdir "/" outfile)
      (lambda (p) (put-c-stubsource p db))))
  ;; emit C apistubs
  (when c (proc c))
  (when cxx (proc cxx))
  ;; emit scm stubs
  (when (or c cxx)
    (call-with-output-file-force
      (string-append scmdir "/" 
                     (output-path/scm (get-scm-libname/libstate db)))
      (lambda (p) (put-scm-stubsource/libstate p db)))

    (call-with-output-file-force
      (string-append scmdir "/"
                     (output-path/scm (get-scm-libname/constants db)))
      (lambda (p) (put-scm-stubsource/constants p db)))))


(define (yuniffistub-app-cmd)
  (define cmd (command-line))
  (define scmdir #f)
  (define cdir #f)
  (define file #f)

  (define (proc-cmd!)
    (when (pair? cmd)
      (let ((a (car cmd))
            (d (cdr cmd)))
        (cond
          ((string=? a "-CDIR")
           (set! cdir (car d))
           (set! cmd (cdr d))
           (proc-cmd!))
          ((string=? a "-SCMDIR")
           (set! scmdir (car d))
           (set! cmd (cdr d))
           (proc-cmd!))
          ((string=? a "-FILE")
           (set! file (car d))
           (set! cmd (cdr d))
           (proc-cmd!))
          (else
            ;; Ignore unknown args for now
            (set! cmd d)
            (proc-cmd!))))))
  (proc-cmd!)

  (cond
    ((and scmdir cdir file)
     (procfile file scmdir cdir))
    (else
      (error "command-line error!" (command-line)))))

)
