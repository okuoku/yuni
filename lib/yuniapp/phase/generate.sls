(library (yuniapp phase generate)
         (export generate-app-cmd)
         (import (yuni scheme)
                 (yuni compat ident)
                 (yuni util files)
                 (yuniapp util openlibfile)
                 (yuniapp util enumlibfiles)
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
(define (generate-app-cmd)
  (define appdir ".")
  (define appsrc "app.sps")
  (define applib "yunilib")
  (define impl (ident-impl))
  (define libs '())
  (define applibpath (string-append appdir "/" applib))
  (define runtimeroot (string-append appdir "/_runtime"))
  (define runtimepath (string-append runtimeroot "/"
                                     (symbol->string impl)))

  (define libsuffix (calc-libsuffix impl))
  (define libgen (calc-generator impl))

  (prepare-dir! runtimeroot)
  (prepare-dir! runtimepath)

  ;; First, enumerate applibs
  (set! libs (enumlibfiles applibpath))

  ;; Generate stub libraries
  (for-each (lambda (f) (gen libgen runtimepath libsuffix f)) libs)
  #t)
)
