(library (yunivm util simplerunner)
         (export 
           simplerunner/expand-program
           simplerunner/treeir-compile
           simplerunner/treeir-run
           new-simplerunner/fakeheap
           new-simplerunner)
         (import (yuni scheme)
                 (yunivm compiler compilercore)
                 (yunivm vm seq-treeir)
                 (yunivm util basiclibs)
                 (yunivm util compatlibs)
                 (yunivm util r7cmapping)
                 (yunivm loader generator)
                 (yunivm expander corelangfilter)
                 (yunivm expander expandcore)
                 (yunivm heap pass)
                 (yunivm heap core)
                 (yunivm heap fake coreops)
                 (yuni compat ident)
                 (yuniconfig build))

         
;; Quickrunner for test

(define libs-proc-vector
  (vector-append basiclibs-proc-vector
                 compatlibs-proc-vector))

(define libs-name-vector
  (vector-append basiclibs-name-vector
                 compatlibs-name-vector))

(define (r7c-noredirect? libname)
  (cond
    ((or (equal? libname '(r7c-basic lib boolean))
         (equal? libname '(r7c-basic lib char))
         (equal? libname '(r7c-basic lib cxr))
         (equal? libname '(r7c-basic lib lists)) 
         (equal? libname '(r7c-basic lib strings)) 
         (equal? libname '(r7c-basic lib vectors)) 
         (equal? libname '(r7c-basic lib bytevectors)) 
         (equal? libname '(r7c-basic lib mapforeach)) 
         (equal? libname '(r7c-equiv std equal)) 
         (equal? libname '(r7c-equiv std lists)) 
         (equal? libname '(r7c-numeric std inexact)) 
         (equal? libname '(r7c-numeric std division)) 
         (equal? libname '(r7c-numeric std generic)) 
         (equal? libname '(r7c-numeric std misc)) 
         (equal? libname '(r7c-io port buffers)) 
         (equal? libname '(r7c-io port control)) 
         (equal? libname '(r7c-io port core)) 
         (equal? libname '(r7c-io port defaults)) 
         (equal? libname '(r7c-io port objects)) 
         (equal? libname '(r7c-yunicore simple-struct)) 
         (equal? libname '(r7c-yunicore yuniport)) 
         )
     #t)
    (else #f)))

(define r7c-libs (map car r7cmapping/stdlib))

(define (r7c-want-fake? libname)
  (and (not (r7c-noredirect? libname))
       (let loop ((libs r7c-libs))
        (and (pair? libs)
             (or (equal? (car libs) libname)
                 (loop (cdr libs)))))))

(define (libmapper libname)
  (cond
    ((r7c-want-fake? libname)
     (cons 'yunifake libname))
    ((or (equal? libname '(r7c-system auxsyntax))
         (equal? libname '(r7c-system core))
         (equal? libname '(r7c-system let-syntax))
         (equal? libname '(r7c-system synrules))
         (equal? libname '(r7c syntax letcore))
         (equal? libname '(r7c syntax definecore))
         (equal? libname '(r7c-basic syntax define))
         (equal? libname '(r7c-basic syntax if))
         (equal? libname '(r7c-basic syntax definecore))
         (equal? libname '(r7c-basic syntax lambda))
         (equal? libname '(r7c-basic syntax letcore))
         ;; Aliases
         (equal? libname '(r7c syntax if))
         (equal? libname '(r7c syntax let))
         (equal? libname '(r7c syntax cond)) 
         (equal? libname '(r7c syntax lambda))
         (equal? libname '(r7c syntax letrec)))
     '(yunivmrt coresyntax))
    ((or (equal? libname '(r7c syntax or))
         (equal? libname '(r7c syntax and)))
     '(yunivmrt scheme-syntax))
    ((or (equal? libname '(r7c heap boolean))
         (equal? libname '(r7c heap procedure))
         (equal? libname '(r7c heap pair))
         (equal? libname '(r7c heap list))
         (equal? libname '(r7c heap eqv))
         (equal? libname '(r7c heap core))
         (equal? libname '(r7c heap char))
         (equal? libname '(r7c heap string))
         (equal? libname '(r7c heap bytevector))
         (equal? libname '(r7c heap symbol))
         (equal? libname '(r7c heap eof-object))
         (equal? libname '(r7c heap vector))
         (equal? libname '(r7c heap undefined))
         (equal? libname '(r7c heap listloop))
         (equal? libname '(r7c heap fixnum))
         (equal? libname '(r7c heap flonum))
         (equal? libname '(r7c-ext simple-struct))
         (equal? libname '(r7c core values))
         (equal? libname '(r7c core apply))
         (equal? libname '(r7c core error))
         (equal? libname '(r7c core callcc))
         (equal? libname '(r7c core exception)))
     (cons 'yunifake libname))
    ((equal? libname '(yuni scheme))
     '(yunivmrt scheme))
    ;; Compat
    ((equal? libname '(yuni compat simple-struct))
     '(yunivmrt yunicore-proc))
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
  (call-with-values (lambda () (compile-core frm (cdr heap)))
                    (lambda (ir mx) ir)))

(define (simplerunner/treeir-run heap ir)
  (filehandle-init!)
  (seq-treeir (car heap) ir))

(define (new-simplerunner) 
  (cons (make-heap-pass (vector->list libs-proc-vector))
        libs-name-vector))

(define (new-simplerunner/fakeheap)
  (let ((name-vector (gen-core-syms-vec))
        (coreops (make-coreops-fake)))
    (cons (make-heap-core coreops name-vector)
          name-vector)))

)
