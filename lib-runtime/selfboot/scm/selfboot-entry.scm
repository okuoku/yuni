(require 'eval)
(require 'srfi-1)
(require 'byte)

(defmacro (define-macro . args) 
  (cons 'defmacro args))

(define-macro (when expr . body)
  `(if ,expr
     (begin ,@body)))

(define-macro (unless expr . body)
  `(if (not ,expr)
     (begin ,@body)))

;; these should be in prelib >>
(define (nan? x) (not (= x x))) ;; FIXME:
(define (inexact x) (exact->inexact x))
(define (exact x) (inexact->exact x))

;;; bytevectors
(define bytevector bytes)
(define make-bytevector make-bytes)
(define bytevector-copy bytes-copy)

;; case-lambda (took from r7rs.scm)
(define-macro (case-lambda . choices)
  `(lambda args
     (case (length args)
       ,@(map (lambda (choice)
                (if (or (symbol? (car choice))
                        (negative? (length (car choice))))
                  `(else (apply (lambda ,(car choice) ,@(cdr choice)) args))
                  `((,(length (car choice)))
                    (apply (lambda ,(car choice) ,@(cdr choice)) args))))
              choices))))

(define bytevector-length bytes-length)

(define bytevector-copy
  (case-lambda
    ((b) (bytes-copy b))
    ((b start) (bytevector-copy b start (bytevector-length b)))
    ((b start end) (subbytes b start end))))

(define bytevector-ref byte-ref)
(define bytevector-set! byte-set!)

(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (- flen start))
             (tmaxcopysize (- tlen at)))
         (bytevector-copy! to at from start (+ start
                                               (min fmaxcopysize
                                                    tmaxcopysize))))))
    ((to at from start end)
     (unless (= start end)
       (let ((b (bytevector-ref from start)))
        (bytevector-set! to at b)
        (bytevector-copy!
          to
          (+ at 1)
          from
          (+ start 1)
          end))))))

(define (bytevector-append . l)
  (list->bytes (apply append (map bytes->list l))))

(define utf8->string
  (case-lambda
    ((b) (bytes->string b))
    ((b start) (utf8->string b start (bytevector-length b)))
    ((b start end) (bytes->string (bytevector-copy b start end)))))

(define string->utf8
  (case-lambda
    ((s) (string->bytes s))
    ((s start) (string->utf8 s start (string-length s)))
    ((s start end) (string->bytes (substring s start end)))))

;; <<


(define (yuni/gensym sym) (gentemp))
(define (yuni/identifier? sym) (symbol? sym))

;; Load

(define myenv (interaction-environment))

(define (eval/filt code env)
  (define (filter-exports exports)
    (filter (lambda (e)
              (case e
                ((_ ... => else unquote unquote-splicing) #f)
                (else #t)))
            exports))
  (define (lib?)
    (and (pair? code)
         (eq? 'library (car code))))
  (if (lib?)
    (let ((libname (cadr code))
          (exports (cdr (caddr code)))
          (imports (cdr (car (cdddr code))))
          (prog (cdr (cdddr code))))
     (eval `(library ,libname
                     (export ,@(filter-exports exports))
                     (import ,@imports)
                     ,@prog)
           env))
    (eval code env)))

(define (myload pth)
  (let ((sexp (%selfboot-file->sexp-list pth)))
   (for-each
     (lambda (exp) 
       ;(write (list 'eval: exp)) (newline)
       (eval/filt exp myenv))
     sexp)))

;; Runtime

(define (%%selfboot-load-aliaslib truename alias* export*)
  ;; Call library runtime
  (let ((lib (yuni/library-lookup truename)))
   (for-each (lambda (name)
               (yuni/library-add-alias! lib name))
             alias*)))

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

(define (%%selfboot-loadlib pth libname imports exports)
  (myload pth))

(define (%%selfboot-load-program pth) (myload pth))

(define (%selfboot-load prefix files)
  (for-each (lambda (e)
              (myload (string-append %%selfboot-yuniroot "/"
                                   prefix "/" e)))
            files))

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
    ;(write (list 'simple cur m q)) (newline)
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
  ;(write %%selfboot-orig-command-line) (newline)
  ;(write %%selfboot-mypath) (newline)
  (let ((npth (%%pathslashfy scmpath)))
   (%%pathsimplify (string-append npth "/../../../.."))))


(define %%selfboot-orig-command-line (program-arguments))
(define %%selfboot-mypath (%%extract-entrypoint-path %%selfboot-orig-command-line))
(define %%selfboot-yuniroot (%%locate-yuniroot-fromscmpath %%selfboot-mypath))
(define %%selfboot-program-args (%%extract-program-args
                                  %%selfboot-orig-command-line
                                  %%selfboot-mypath))


(define %%selfboot-impl-type 'scm)
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
                               (yuni scheme)
                               ))

(define (%%selfboot-error-hook c)
  (display "ERROR!\n" (trace-output-port))
  (write-condition-report c (trace-output-port))
  (newline (trace-output-port))
  (exit 1))


(when (string=? %%selfboot-yuniroot "")
  (set! %%selfboot-yuniroot "."))

(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define (command-line) %%selfboot-current-command-line)

(define %%selfboot-mainprog #f)

(myload (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/common.scm"))
(myload (string-append %%selfboot-yuniroot "/lib-runtime/selfboot/common/run-program.scm"))

(exit 0)
