(library (yunivm util simplerunner)
         (export 
           simplerunner/expand-program
           simplerunner/treeir-compile
           simplerunner/treeir-run
           new-simplerunner)
         (import (yuni scheme)
                 (yunivm compiler compilercore)
                 (yunivm vm seq-treeir)
                 (yunivm util basiclibs)
                 (yunivm util compatlibs)
                 (yunivm loader generator)
                 (yunivm expander corelangfilter)
                 (yunivm expander expandcore)
                 (yunivm heap pass)
                 (yuni compat ident)
                 (yuniconfig build))

         
;; Quickrunner for test

(define libs-proc-vector
  (vector-append basiclibs-proc-vector
                 compatlibs-proc-vector))

(define libs-name-vector
  (vector-append basiclibs-name-vector
                 compatlibs-name-vector))

(define (libmapper libname)
  (cond
    ((or (equal? libname '(r7c-system auxsyntax))
         (equal? libname '(r7c-system core))
         (equal? libname '(r7c-system let-syntax))
         (equal? libname '(r7c-system synrules))
         (equal? libname '(r7c-basic syntax define))
         (equal? libname '(r7c-basic syntax if))
         (equal? libname '(r7c-basic syntax definecore))
         (equal? libname '(r7c-basic syntax lambda))
         (equal? libname '(r7c-basic syntax letcore))
         ;; Aliases
         (equal? libname '(r7c syntax lambda))
         (equal? libname '(r7c syntax letcore))
         (equal? libname '(r7c syntax letrec))
         (equal? libname '(r7c syntax if))
         (equal? libname '(r7c syntax definecore)))
     '(yunivmrt coresyntax))
    ((or (equal? libname '(r7c heap boolean))
         (equal? libname '(r7c heap pair))
         (equal? libname '(r7c heap list))
         (equal? libname '(r7c heap vector))
         (equal? libname '(r7c heap undefined))
         (equal? libname '(r7c heap listloop))
         (equal? libname '(r7c heap fixnum))
         (equal? libname '(r7c core values))
         (equal? libname '(r7c core apply))
         (equal? libname '(r7c core error))
         (equal? libname '(r7c core callcc))
         (equal? libname '(r7c core exception)))
     '(yunivmrt coreprocs))
    ((equal? libname '(yuni scheme))
     '(yunivmrt scheme))
    ;; Compat
    ((equal? libname '(yuni compat simple-struct))
     '(yunifake compatlibs))
    ((equal? libname '(yuni compat keywords))
     '(yunivmrt keywords))
    ((and (< 2 (length libname)) 
          (eq? (car libname) 'yuni)
          (eq? (cadr libname) 'compat))
     ;; Disallow compat libraries
     (cons 'NEVERLAND libname))
    (else
      (let ((head (car libname)))
       (case head
         ;; Disallow direct-access of r7c libraries
         ((r7c r7c-system)
          (cons 'NEVERLAND libname))
         (else libname))))))

(define (simplerunner/expand-program heap prog)
  (define (output code arg* modpath do-dump use-debugger)
    (corelangfilter (expand0 code)))
  (define arg*
    (list 
      "-I"
      (string-append (yuniconfig-runtime-rootpath) "/"
                     (symbol->string (ident-impl)))))
  (yuniloader-generate arg* libmapper prog output))

(define (simplerunner/treeir-compile heap frm)
  (call-with-values (lambda () (compile-core frm libs-name-vector))
                    (lambda (ir mx) ir)))

(define (simplerunner/treeir-run heap ir)
  (seq-treeir heap ir))

(define (new-simplerunner) 
  (make-heap-pass (vector->list libs-proc-vector)))

)
