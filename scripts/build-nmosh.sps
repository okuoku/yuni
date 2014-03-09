;; Library build script for nmosh

(import (rnrs) 
        (match) 
        (yuni util files)
        (yuni core)
        (only (mosh pp) pp)
        )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Library reader/generator main
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define STUBTOP "lib-stub")
(define (generate libraries gen recipe)
  (define (genbody dirsym groups)
    (define dirname (path-append STUBTOP (symbol->string dirsym)))
    (when (not (file-exists? STUBTOP))
      (make-directory STUBTOP))
    (when (not (file-exists? dirname))
      (make-directory dirname))
    )
  (for-each (lambda (e) (genbody (car e) (cdr e)))
            recipe))

(define (read-library pth) ;; => (pth library-name . body)
  (define (realize sexp)
    (match lib
           (('library libname data ...)
            (cons libname lib))))
  (cons pth
        (realize (file->sexp-list pth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Library generators
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-output-to-file-force file proc)
  (when (file-exists? file)
    (delete-file file))
  (with-output-to-file file proc))

;; GenRacket: R6RS library generator for Racket

(define (libgen-racket-body libname exports imports libpath)
  `(library ,libname
             (export ,@exports)
             (import ,@imports
                     (only (racket include) include)
                     (only (rnrs) syntax-rules _ ... begin))
    (define-syntax %%-internal-paste
      (syntax-rules ()
        ((_ (library  (export ...) (import ...) body ...))
         (begin body ...))))
    (%%-internal-paste
      (include libpath))))

(define (libgen-racket libcode libpath outputpath)
  (match lib
         (('library libname 
           ('export exports ...)
           ('import imports ...) 
           body ...)
          (with-output-to-file-force 
            outputpath
            (lambda (p)
              (define body (libgen-racket-body libname exports imports libpath))
              (put-string p "#!r6rs\n")
              (pp body p))))))

;; GenR7RS: R7RS library generator 

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
                        (('var obj ...) 
                         (set! var obj))
                        (else 'do-nothing)))
               config))))

(read-var *library-directries*)
(read-var *library-groups*)
(read-var GenRacket)
(read-var GenR7RS)

;; Collect libraries
(define (collect-libraries dir)
  (define files '())
  (define (library? pth)
    (let ((e (path-extension pth)))
      ;; FIXME: Do we need more?
      (or (string=? e "sls"))))
  ;; Recursively collect files on the dir
  (directory-walk dir (lambda (file) 
                        (when (library? file) 
                          (set! files (cons file files)))))
  (set! libraries (append libraries
                          (map read-library files))))

(for-each (lambda (dir)
            (collect-libraries dir))
          *library-directories*)

;; Generate !
(generate libraries libgen-racket GenRacket)
; (generate libgen-r7rs GenR7RS)
