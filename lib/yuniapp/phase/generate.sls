(library (yuniapp phase generate)
         (export generate-app-cmd)
         (import (yuni scheme)
                 (yuni compat ident)
                 (yuni util files)
                 (yuniconfig build)
                 (yuniapp util openlibfile)
                 (yuniapp util enumlibfiles)
                 (yuniapp tmpl cmdline)
                 (yuniapp tmpl r6rs))

(define (calc-libsuffix sym)
  (case sym
    ((racket) ".mzscheme.sls")
    (else 
      (error "Unknown implementation" sym))))

(define (calc-generator sym)
  (case sym
    ((racket) tmpl-r6rs/racket)
    (else
      (error "Unknown implementation" sym))))

(define (gen generator runtimepath suffix libfile)
  (write (list 'generating: libfile))
  (newline)
  (let ((lib (file->sexp-list libfile)))
   (unless (and lib (pair? lib) (= 1 (length lib)))
     (error "Malformed library" libfile))

   (let* ((lib0 (car lib))
          (libname (cadr lib0))
          (exports (cdr (caddr lib0)))
          (imports (cdr (cadddr lib0))))
     (let ((p (openlibfile #t libname runtimepath suffix)))
      (display (generator libname exports imports libfile) p)
      (close-port p)))))

(define (prepare-dir! pth)
  (unless (file-directory? pth)
    (create-directory pth)))

;; FIXME: Tentative
(define (pickup-dir opt)
  (define (itr l)
    (cond
      ((and (pair? l) (string=? opt (car l))
            (pair? (cdr l)))
       (cadr l))
      ((pair? l)
       (itr (cdr l)))
      (else ".")))

  (let ((cmd (command-line)))
   (itr cmd)))

(define (removetrail str)
  ;; Remove trailing (back)slash
  (let* ((len (string-length str))
         (c (string-ref str (- len 1))))
    (if (or (char=? c #\\ ) (char=? c #\/))
      (substring str 0 (- len 1))
      str)))

(define (use-batchfile?)
  (let ((o (yuniconfig-platform)))
   (or (string=? "WIN32" o)
       (string=? "WIN64" o))))

(define (generate-app-cmd)
  (define batchfile? (use-batchfile?))
  (define appdir (removetrail (pickup-dir "-GENERATE")))
  (define gendir (removetrail (pickup-dir "-CURRENTDIR")))
  (define appsrc (string-append appdir "/" "app.sps"))
  (define applib "yunilib")
  (define impl (ident-impl))
  (define libs '())
  (define applibpath (string-append appdir "/" applib))
  (define runtimeroot (string-append gendir "/_runtime"))
  (define runtimepath (string-append runtimeroot "/"
                                     (symbol->string impl)))
  (define runscript (string-append gendir "/" 
                                   "run-" (symbol->string impl)
                                   (if batchfile? ".bat" ".sh")))

  (define libsuffix (calc-libsuffix impl))
  (define libgen (calc-generator impl))

  ;; Check app.sps
  (unless (file-exists? appsrc)
    (error "app.sps not found" appsrc))

  ;; Check appdir
  (unless (file-directory? appdir)
    (error "appdir is not a directory" appdir))

  ;; Check gendir
  (unless (file-directory? gendir)
    (error "gendir is not a directory" gendir))
  
  (prepare-dir! runtimeroot)
  (prepare-dir! runtimepath)

  ;; First, enumerate applibs
  (set! libs (enumlibfiles applibpath))

  ;; Generate stub libraries
  (for-each (lambda (f) (gen libgen runtimepath libsuffix f)) libs)

  ;; Generate launch commandline
  (when (file-exists? runscript) ;; Not required actually
    (delete-file runscript))
  (string-list->file runscript
                     (list 
                       ((if batchfile? 
                          cmdline-win32
                          #f)
                        impl
                        (list runtimepath)
                        appsrc
                        '())))
  )
)
