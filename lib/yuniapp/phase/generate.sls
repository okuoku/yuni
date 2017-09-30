(library (yuniapp phase generate)
         (export generate-app-cmd)
         (import (yuni scheme)
                 (yuni compat ident)
                 (yuni util files)
                 (yuniconfig build)
                 (yuniapp util openlibfile)
                 (yuniapp util enumlibfiles)
                 (yuniapp util tmplutils)
                 (yuniapp tmpl cmdline)
                 (yuniapp tmpl r7rs)
                 (yuniapp tmpl r6rs))

(define (calc-libsuffix sym)
  (case sym
    ((racket) ".mzscheme.sls")
    ((guile) ".scm")
    ((chibi-scheme gauche sagittarius) ".sld")
    ((chez vicare) ".sls")
    (else 
      (error "Unknown implementation" sym))))

(define (tmpl-do-nothing . bogus) #t)

(define (calc-generator sym)
  (case sym
    ((racket) tmpl-r6rs/racket)
    ((guile) tmpl-r6rs/guile)
    ((chibi-scheme) tmpl-r7rs/chibi-scheme)
    ((gauche sagittarius) tmpl-r7rs/generic-fullpath)
    ((chez vicare) tmpl-do-nothing)
    (else
      (error "Unknown implementation" sym))))

(define (use-batchfile?)
  (let ((o (yuniconfig-platform)))
   (or (string=? "WIN32" o)
       (string=? "WIN64" o))))

(define (use-rootrelative? sym) ;; Do direct load?
  (case sym
    ((chibi-scheme chez vicare) #t)
    (else #f)))

(define (do-strip-keywords?)
  (let ((o (ident-impl)))
   (case o
     ((gauche sagittarius) #t)
     (else #f))))

(define (do-strip-stdaux?)
  (let ((o (ident-impl)))
   (case o
     ((chicken guile) #t)
     (else #f))))

(define (gen generator prepare-dirs!
             runtimepath suffix libfile strip-keywords? strip-stdaux?)
  (let ((lib (file->sexp-list libfile)))
   (unless (and lib (pair? lib) (= 1 (length lib)))
     (error "Malformed library" libfile))

   (let* ((lib0 (car lib))
          (libname (cadr lib0))
          (exports (cdr (caddr lib0)))
          (imports (cdr (cadddr lib0))))
     ;; Strip exports
     (let* ((a (if strip-keywords? 
                 (filter-keyword exports)
                 exports))
            (e (if strip-stdaux?
                 (filter-stdaux a)
                 a)))
       ;; Output
       (let ((content (generator libname e imports libfile)))
        (define (generate sym)
          (let ((p (openlibfile #t libname runtimepath suffix)))
           (write (list sym libfile))
           (newline)
           (display content p)
           (close-port p)))
        (when (string? content)
          (prepare-dirs!)
          (let ((pold (openlibfile #f libname runtimepath suffix)))
            (cond
              (pold
                (let* ((pnew (open-input-string content))
                       (old (read pold))
                       (new (read pnew)))
                  (close-port pold)
                  (close-port pnew)
                  (cond
                    ((equal? old new)
                     ;; Skip output
                     (write (list 'skip: libfile))
                     (newline))
                    (else (generate 'Update:)))))
              (else (generate 'Generate:))))))))))

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

(define (generate-app-cmd) ;; Closes output port
  (define impl (ident-impl))
  (define batchfile? (use-batchfile?))
  (define rootrelative? (use-rootrelative? impl))
  (define strip-keywords? (do-strip-keywords?))
  (define strip-stdaux? (do-strip-stdaux?))
  (define appdir (removetrail (pickup-dir "-GENERATE")))
  (define gendir (removetrail (pickup-dir "-CURRENTDIR")))
  (define appsrc (string-append appdir "/" "app.sps"))
  (define applib "yunilib")
  (define libs '())
  (define applibpath (string-append appdir "/" applib))
  (define loaderroot (removetrail (yuniconfig-loader-rootpath)))
  (define runtimeroot (string-append gendir "/_runtime"))
  (define stublibpath (string-append gendir "/_yunistub"))
  (define runtimepath (string-append runtimeroot "/"
                                     (symbol->string impl)))
  (define runtimemodpath (yuniconfig-platform-stubdir))
  (define runscript (string-append gendir "/" 
                                   "run-" (symbol->string impl)
                                   (if batchfile? ".bat" ".sh")))

  (define libpath (if rootrelative?
                    (list applibpath runtimepath runtimemodpath)
                    (list runtimepath runtimemodpath)))

  (define libsuffix (calc-libsuffix impl))
  (define libgen (calc-generator impl))
  (define dir-prepared? #f)
  (define (prepare-dirs!)
    (unless dir-prepared?
      (prepare-dir! runtimeroot)
      (prepare-dir! runtimepath)
      (set! dir-prepared? #t)))

  ;; Check app.sps
  (unless (file-exists? appsrc)
    (error "app.sps not found" appsrc))

  ;; Check appdir
  (unless (file-directory? appdir)
    (error "appdir is not a directory" appdir))

  ;; Check gendir
  (unless (file-directory? gendir)
    (error "gendir is not a directory" gendir))

  ;; First, enumerate applibs
  (when (file-directory? applibpath)
    (set! libs (enumlibfiles applibpath)))

  ;; Check stubdir
  (when (file-directory? stublibpath)
    (let ((stublibs (enumlibfiles stublibpath)))
     (write (list 'stubs: stublibs)) (newline)
     (unless (null? stublibs)
       (set! libs (append libs stublibs))
       (set! libpath (append libpath (list stublibpath))))))

  ;; Generate stub libraries
  (for-each (lambda (f) (gen libgen 
                             prepare-dirs!
                             runtimepath libsuffix f
                             strip-keywords? strip-stdaux?)) libs)

  ;; Generate launch commandline
  (when (file-exists? runscript) ;; Not required actually
    (delete-file runscript))
  (string-list->file runscript
                     (list 
                       ((if batchfile? 
                          cmdline-win32
                          cmdline-sh)
                        impl
                        loaderroot
                        appdir
                        libpath
                        appsrc
                        '())))
  (close-port (current-output-port)))
)
