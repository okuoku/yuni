;; Library build script for nmosh

(import (rnrs) 
        (match) 
        (yuni util files)
        (yuni core)
        (only (mosh pp) pp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Library reader/generator main
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STUBTOP "lib-stub")
(define (generate libraries gen recipe)
  (define (genbody dirsym groups)
    (define dirname (path-append STUBTOP (symbol->string dirsym)))
    (define (gengroup group)
      (for-each (lambda (m)
                  (match m
                         ((grp srcpath (name . alias) code)
                          (when (eq? grp group)
                            (gen name alias code srcpath dirname dirsym)))
                         (else
                           (display (list "WARNING: ignored " (cadr m)))
                           (newline))))
        *library-map*))
    (when (not (file-exists? STUBTOP))
      (create-directory STUBTOP))
    (when (not (file-exists? dirname))
      (create-directory dirname))
    (for-each gengroup groups))
  (for-each (lambda (e) (genbody (car e) (cdr e)))
            recipe))

(define (read-library pth) ;; => (pth library-name . body)
  (define (realize sexp)
    (match sexp
           ((('library libname data ...))
            (cons libname sexp))
           (else
             (assertion-violation #f
                                  "invalid library format"
                                  sexp))))
  (cons pth
        (realize (file->sexp-list pth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Library generators
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

(define (calc-libpath basepath sexp ext)
  (string-append (fold-left (lambda (cur e)
                              (path-append cur (symbol->string e)))
                            basepath
                            sexp)
                 "."
                 ext))

(define (strip-rename lis)
  (fold-left (lambda (cur e)
               (match e
                      (('rename renames ...)
                       (append (map cadr renames) cur))
                      (otherwise
                        (cons otherwise cur))))
             '()
             lis))

(define (calc-relative libname libpath)
  (define dots (fold-left (lambda (cur e)
                            (string-append cur "../"))
                          ""
                          (cdr libname)))
  (string-append dots "../../" libpath))

;; GenRacket: R6RS library generator for Racket

(define (libgen-racket-body libname exports imports libpath)
  `(library ,libname
             (export ,@exports)
             (import ,@imports
                     (yuni-runtime racket))
    (%%internal-paste ,libpath)))

(define (libgen-racket-alias from to syms)
  `(library ,to
            (export ,@syms)
            (import ,from)))

(define (libgen-racket name alias libcode libpath basepath flavor)
  (define (base0filter sexp)
    ;; Duh! Racket has (scheme base)!!!
    (if (equal? sexp '(scheme base))
      '(scheme base0)
      ;; Oh, (scheme file)..
      (if (equal? sexp '(scheme file))
        '(scheme file0)
        sexp)))
  (define outputpath (calc-libpath basepath name "mzscheme.sls"))
  (define aliaspath (and alias (calc-libpath 
                                 basepath (base0filter alias) "mzscheme.sls")))
  (match libcode
         (('library libname 
           ('export exports ...)
           ('import imports ...) 
           body ...)
          (call-with-output-file-force
            outputpath
            (lambda (p)
              (define body (libgen-racket-body name exports 
                                               (map base0filter imports) 
                                               libpath))
              (put-string p "#!r6rs\n")
              (pp body p)))
          (when alias
            (call-with-output-file-force
              aliaspath
              (lambda (p)
                (define body (libgen-racket-alias name alias (strip-rename
                                                               exports)))
                (put-string p "#!r6rs\n")
                (pp body p)))))
         (else
           (assertion-violation #f "Invalid library format" libcode)) ))

;; GenR7RS: R7RS library generator 
(define (libgen-r7rs-body libname exports imports libpath flavor)
  (define calclibpath (if (eq? flavor 'gauche)
                        (lambda (_ x) x)
                        calc-relative))
  `(define-library ,libname
             (export ,@exports)
             (import ,@imports (yuni-runtime r7rs))
             (include ,(calclibpath libname libpath))))

(define (libgen-r7rs-alias from to syms)
  `(define-library ,to
            (export ,@syms)
            (import ,from)))

(define (libgen-r7rs name alias libcode libpath basepath flavor)
  (define LIBEXT (if (eq? flavor 'gauche) "scm" "sld"))
  (define outputpath (calc-libpath basepath name LIBEXT))
  (define aliaspath (and alias (calc-libpath 
                                 basepath alias LIBEXT)))
  (define (may-strip-keywords lis)
    (define (keyword-symbol? sym)
      (let ((c (string-ref (symbol->string sym) 0)))
        (char=? #\: c)))
    (if (eq? flavor 'gauche)
      (fold-left (lambda (cur e) 
                   (if (keyword-symbol? e)
                     cur
                     (cons e cur)))
                 '()
                 lis)
      lis))
  (match libcode
         (('library libname 
           ('export exports ...)
           ('import imports ...) 
           body ...)
          (call-with-output-file-force
            outputpath
            (lambda (p)
              (define body (libgen-r7rs-body name 
                                             (may-strip-keywords exports) 
                                             imports
                                             libpath
                                             flavor))
              (pp body p)))
          (when alias
            (call-with-output-file-force
              aliaspath
              (lambda (p)
                (define body (libgen-r7rs-alias name alias 
                                                (may-strip-keywords
                                                  (strip-rename exports))))
                (pp body p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define config (file->sexp-list "config/config.scm"))

(define *library-directories* '())
(define *library-groups* '())
(define GenRacket '())
(define GenR7RS '())
(define libraries '())

(define-syntax read-var
  (syntax-rules () 
    ((_ var)
     (for-each (lambda (e) 
                 (match e
                        (('var . obj) 
                         (set! var obj))
                        (else 'do-nothing)))
               config))))

(read-var *library-directories*)
(read-var *library-groups*)
(read-var GenRacket)
(read-var GenR7RS)

;; Collect libraries
(define (collect-libraries dir)
  (define files '())
  (define (library? pth)
    (let ((e (path-extension pth)))
      ;; FIXME: Do we need more?
      (and e
           (or (string=? e "sls")))))
  ;; Recursively collect files on the dir
  (directory-walk dir (lambda (file) 
                        (when (library? file) 
                          (set! files (cons file files)))))
  (set! libraries (append libraries
                          (map read-library files))))

(for-each (lambda (dir)
            (collect-libraries dir))
          *library-directories*)

;; Map library files
;;    map = (groupsym sourcepath (outname . aliased) . libcode)
(define (libname->groupname libname)
  (define libsym (car libname))
  (define (matchnames ret names)
    (and (pair? names)
         (let ((name (car names))
               (next (cdr names)))
           (or (and (eq? (if (pair? name) (car name) name)
                         libsym)
                    ret)
               (matchnames ret next)))))
  (define (itr cur)
    (match cur
           (((name . groups) . next)
            (or (matchnames name groups)
                (itr next)))
           (else #f)))
  (itr *library-groups*))

(define (libname->outname+aliased? libname)
  (define libsym-top (car libname))
  (define libsym-sub (cdr libname))
  (define (ret name alias)
    (cons (cons name libsym-sub) 
          (and alias (cons alias libsym-sub))))
  (define (alias entries)
    (match entries
           (((name '=> truename) . next)
            (or (and (eq? name libsym-top)
                     (ret name truename))
                (alias next)))
           ((name . next)
            (or (and (eq? name libsym-top)
                     (ret name #f))
                (alias next) ))
           (else #f)))
  (define (itr cur)
    (and (pair? cur)
         (let ((entries (cdar cur))
               (next (cdr cur)))
           (or (alias entries)
               (itr next)))))
  (itr *library-groups*))

(define *library-map*
  (map 
    (lambda (lib) 
      (match lib
             ((pth libname . code)
              (cons (libname->groupname libname)
                    (cons pth
                          (cons (libname->outname+aliased? libname)
                                code))))))
    libraries))

;; Generate !
(generate libraries libgen-racket GenRacket)
(generate libraries libgen-r7rs GenR7RS)
