;; FIXME: Support non-standards ~ and :=
;;
;; "FAKE" yuniloader, try to lower yuni scripts into R5RS
;;
;; - Library symbol manipulation `except`, `only` and `rename` is not supported
;; - For macro exporting library, no namespacing will be provided
;;
;; (%%yuniloader-fake-generate args* k)
;;  => (k sexp arg* modpath do-dump)
;;
;; ARGS:
;; 
;;  -VERBOSE     -- Turn on verbose output
;;  -DUMP        -- Do not execute, dump expanded code instead
;;  -MOD <str>   -- Platform module path
;;  -I <str>     -- append to library search path
;;
;; SPECIAL SYNTAX:
;;
;;  - (define-primitive-names/yunifake sym ...)
;;  - (define-syntax-names/yunifake sym ...)
;; 
;; SPECIAL LIBRARY:
;; 
;;  - (yunifake . anything) - Ignored

(define (%%yuniloader-fake-generate input-args* k)

  (define FIXME-DEFAULT-MACROS '(~ :=))

  ;; Library data
  ;; 
  ;;  <LIB> = #(libname libfile libcontent has-macro? <exports>* <imports>*)
  ;;  <exports> = (sourcename exportname/#f origlib)  --- #f for macro
  ;;  <imports> = (sourcename exportname/#f sourcelib origlib)

  (define loaded-libraries '()) ;; = (<LIB> ...)
  (define loaded-syms '()) ;; alist of top-level-defined symbols

  ;; FIXME: Use this!!
  (define (require-loaded-syms! sym lib) ;; => #t / #f / error
    (let ((xlib (assq sym lib)))
     (cond
       ((not xlib) ;; New one
        (set! loaded-syms
          (cons (cons sym lib)
                loaded-syms)) 
        ;; Require a (define ...) line
        #t)
       ((equal? xlib lib)
        ;; Already exists
        #f)
       (else
         (error "Unmatched global import" xlib lib)))))


  (define modpath #f)
  (define program-file #f)
  ;; -LIB option
  (define import-dirs '())
  (define arg* '())

  (define (filter-sequence seq)
    (define (itr acc rest)
      (if (pair? rest)
        (let ((frm (car rest))
              (next (cdr rest)))
          (if (and (pair? frm) 
                   (or (eq? 'define-primitive-names/yunifake (car frm))
                       (eq? 'define-syntax-names/yunifake (car frm))))
            (itr acc next)
            (itr (cons frm acc) next)))
        (let ((r (reverse acc)))
         r)))
    (itr '() seq))

  (define (run)
    (define (filter/reverse-output seq)
      ;; Remove () and (begin) from output and reverse
      (define (itr acc rest)
        (if (pair? rest)
          (let ((frm (car rest))
                (next (cdr rest)))
            (if (or (null? frm)
                    (and (pair? frm) (eq? 'begin (car frm))
                         (null? (cdr frm))))
              (itr acc next)
              (itr (cons frm acc) next)))
          acc))
      (itr '() seq))
    (define (filter-imports lis)
      (define (itr acc cur)
        (if (pair? cur)
          (let ((e (car cur))
                (next (cdr cur)))
            (let ((to (car e))
                  (from (cadr e)))
              (cond
                ((or (not from) (eq? to from))
                 (itr acc next))
                (else
                  (itr (cons e acc) next)))))
          acc))
      (itr '() lis))

    (define (gen-code lib)
      (define libname (vector-ref lib 0))
      (define file (vector-ref lib 1))
      (define seq (vector-ref lib 2))
      (define has-macro? (vector-ref lib 3))
      (define exports (vector-ref lib 4))
      (define imports (vector-ref lib 5))
      (define (filter-exports lis)
        (define (itr acc cur)
          (if (pair? cur)
            (let ((e (car cur))
                  (next (cdr cur))
                  (from (caar cur))
                  (to (cadar cur))
                  (elib (caddar cur)))
              (if (not (equal? elib libname))
                (itr acc next)
                (itr (cons e acc) next)))
            acc))
        (itr '() lis))
      (if has-macro?
        (gen-libbody/naked file (filter-imports imports) seq)
        (gen-libbody file 
                     (filter-imports imports) 
                     (filter-exports exports) seq)))
    (let ((code (map gen-code loaded-libraries)))
     (k (filter/reverse-output code) arg* modpath %do-dump)))

  (define (parseargs! lis)
    (cond
      ((pair? lis)
       (let ((a (car lis))
             (d (cdr lis)))
         (cond
           ((string=? "-VERBOSE" a)
            (set! %verbose #t)
            (parseargs! d))
           ((string=? "-DUMP" a)
            (set! %do-dump #t)
            (parseargs! d))
           ((string=? "-I" a)
            (let ((pth (car d))
                  (next (cdr d)))
              (set! import-dirs (cons pth import-dirs))
              (parseargs! next)))
           ((string=? "-MOD" a)
            (let ((pth (car d))
                  (next (cdr d)))
              (set! modpath pth)
              (parseargs! next)))
           (else 
             (set! arg* lis)))))))

  (define namectr 10)
  (define (gen-exportsym basename)
    (set! namectr (+ 1 namectr))
    (string->symbol
      (string-append
        "%%%export-"
        (symbol->string basename)
        "~"
        (number->string namectr))))

  (define (gen-libbody lib renames exports body)
    (define (gen-renames lis)
      (map (lambda (e)
             (let ((to (car e))
                   (from (cadr e)))
               (list to from)))
           lis))
    `(begin 
       ,@(map (lambda (e)
                (let ((name (cadr e)))
                 `(define ,name #f)))
              exports)
       ,@(if (and (null? body) (null? exports))
          '()
          `((let ,(gen-renames renames) 
            ,@body ,@(map (lambda (e)
                            (let ((from (car e))
                                  (to (cadr e)))
                              `(set! ,to ,from)))
                          exports))))))

  (define (gen-libbody/naked lib renames body)
    `(begin 
       ,@(map (lambda (e)
                (let ((to (car e))
                      (from (cadr e)))
                  `(define ,to ,from)))
              renames)
       . ,body))

  (define ERRPORT current-error-port)
  (define %verbose #f)
  (define %do-dump #f)
  (define (PCK . obj)
    (if %verbose
      (begin
        (if #t ;; (not DEBUGGING)
          (begin
            (display "-> " (ERRPORT))
            (for-each (lambda (e)
                        (write e (ERRPORT))
                        (display " " (ERRPORT)))
                      obj)
            (newline (ERRPORT)))))))

  (define (file->sexp-list pth)
    (define (itr p cur)
      (let ((obj (read p)))
       (if (eof-object? obj)
         (reverse cur)
         (itr p (cons obj cur)))))
    (call-with-input-file
      pth
      (lambda (p) (itr p '()))))

  (define (make-library-path base nam)
    ;(PCK 'make-library-path: base nam)
    (if (pair? nam)
      (make-library-path (string-append (string-append base "/") 
                                        (symbol->string (car nam))) 
                         (cdr nam))
      (string-append base ".sls")))

  (define (builtin-library? nam)
    (and (pair? nam)
         (let ((prefix (car nam)))
          (case prefix
            ((yunifake) #t)
            (else #f)))))

  (define (library-name->path name)
    (define (itr rest)
      (if (pair? rest)
        (or (let ((name (make-library-path (car rest) name)))
             (PCK 'TRYING: name)
             (and (file-exists? name)
                  name))
            (itr (cdr rest)))
        (error "library-name->path: Cannot find library for" name)))
    (PCK 'LOOKUP: name)
    (itr import-dirs))

  (define (library-loaded? nam)
    (define (itr rest)
      (and (pair? rest)
           (or (equal? nam (caar rest))
               (itr (cdr rest)))))
    (itr loaded-libraries))

  (define (mark-as-loaded! nam filename content has-macro? exports imports)
    (set! loaded-libraries 
      (cons (vector nam filename (filter-sequence content) 
                    has-macro? exports imports) 
            loaded-libraries)))

  (define (library-lookup-exports name) ;; => nil for embedded library (ignore)
    (cond
      ((builtin-library? name)
       '())
      (else
        (letrec ((itr (lambda (rest)
                        (if (pair? rest)
                          (if (equal? (vector-ref (car rest) 0) name)
                            (vector-ref (car rest) 4)
                            (itr (cdr rest)))
                          (begin ;; Load specified library automagically
                            (let ((pth (library-name->path name)))
                             (load-library-file! pth)
                             (library-lookup-exports name)))))))
          (itr loaded-libraries)))))

  (define (process-import-clause lis)
    (cond ((pair? lis)
           (cond ((or (eq? (car lis) 'only)
                      (eq? (car lis) 'except)
                      (eq? (car lis) 'rename))
                  ;; FIXME: Support them
                  (error "process-import-clause: Unsupported format" lis))
                 (else
                   (let ((exports (library-lookup-exports lis)))
                    (map (lambda (e)
                           (let ((srcname (car e))
                                 (expname (cadr e))
                                 (origlib (caddr e)))
                             ;; Add sourcelib
                             (list srcname expname lis origlib)))
                         exports)))))
          (else
            (error "process-import-clause: Malformed libname" lis))))

  (define (process-sequence! pth libname import-clauses export-clauses sequence)
    (define exports '())
    (define imports '())
    (define macros FIXME-DEFAULT-MACROS)
    (define primitives '())
    (define has-macro-export? #f)

    (define (macro? sym) (memq sym macros))
    (define (primitive? sym) (memq sym primitives))
    (define (lookup-env sym) ;; => sym prim, #t macro, otherwise import
      (define (filter-import-macro import)
        ;; Return #t if the import entry were a macro
        (if (import-macro? import)
          #t
          import))
      (define (itr rest)
        (and (pair? rest)
             (or (and (eq? sym (caar rest)) (filter-import-macro (car rest)))
                 (itr (cdr rest)))))
      (cond
        ((macro? sym) #t)
        ((primitive? sym) sym)
        (else
          (itr imports))))

    (define (import-macro? e) (eq? (cadr e) #f))

    ;; First, scan over sequence and collect special keywords
    ;;
    ;; (define-syntax name ...) => register name for a macro
    ;; (define-primitive-names/yunifake name ...) => register names
    ;; (define-syntax-names/yunifake name ...) => register names
    ;; (begin seq ...) => dig down
    (define (scan-form frm)
      (cond
        ((pair? frm)
         (let ((top (car frm)))
          (cond
            ((eq? 'define-syntax top)
             (let ((name (cadr frm)))
              (set! macros (cons name macros))))
            ((eq? 'define-primitive-names/yunifake top)
             (let ((names (cdr frm)))
              (set! primitives (append names primitives))))
            ((eq? 'define-syntax-names/yunifake top)
             (let ((names (cdr frm)))
              (set! macros (append names macros))))
            ((eq? 'begin top)
             (for-each scan-form (cdr frm))))))))

    (for-each scan-form sequence)

    ;; Generate import list
    (for-each (lambda (cls)
                (let ((import (process-import-clause cls)))
                 (set! imports (append import imports))))
              import-clauses)

    ;; Filter import list -- Remove re-export entries
    (let ((to-check imports)
          (checked '()))
      (letrec ((itr  ;; Collect the samenames
                 (lambda (name acc root nexacc rest)
                   (cond
                     ((pair? rest)
                      (let* ((e (car rest))
                             (ename (car e))
                             (next (cdr rest)))
                        (cond
                          ((eq? name ename)
                           (let ((srclib (caddr e))
                                 (origlib (cadddr e)))
                             (cond
                               ((or (not origlib) (equal? srclib origlib))
                                ;; It's candidate for the root entry
                                (itr name acc e nexacc next))
                               (else
                                 (itr name (cons e acc) root nexacc next)))))
                          (else
                            (itr name acc root (cons e nexacc) next)))))
                     (else
                       (cond
                         (root
                           ;; Only the root will survive
                           (set! checked (cons root checked)))
                         ((= 1 (length acc))
                          ;; length > 1 means some conflict
                          (set! checked (cons (car acc) checked)))
                         (else
                           (error "process-sequence!: Conflict" acc)))
                       ;; Go to next itr.
                       (set! to-check nexacc)
                       (pop)))))
               (pop (lambda ()
                      (and (pair? to-check)
                           (let ((name (caar to-check)))
                            (itr name '() #f '() to-check))))))
        (pop))
      (set! imports checked))

    ;; Generate export list
    (letrec 
      ((itr 
         (lambda (rest)
           (cond
             ((pair? rest)
              (let ((sym (car rest)))
               (cond
                 ((not (symbol? sym))
                  (error "Malformed export" sym)))
               (let ((import (lookup-env sym)))
                (cond
                  ((eq? #t import) ;; macro
                   (set! has-macro-export? #t)
                   (set! exports (cons (list sym #f #f) exports)))
                  ((symbol? import) ;; Primitive
                   (set! exports (cons (list sym sym #f) exports)))
                  (import ;; Symbols from other libs
                    (cond
                      ((import-macro? import)
                       (set! exports (cons (list sym #f #f) exports)))
                      (else
                        (let ((gname (cadr import))
                              (origlib (cadddr import)))
                         (set! exports 
                           (cons (list sym gname origlib) exports))))))
                  (else ;; Defined in me, assign a global name
                    (let ((gname (gen-exportsym sym)))
                     (set! exports (cons (list sym gname libname) exports)))))

                (itr (cdr rest)))))))))
      (itr export-clauses))

    ;; Un-rename export list if we were has-macro-export?
    (cond
      (has-macro-export?
        (set! exports
          (map (lambda (e)
                 (let ((sym (car e))
                       (gname (cadr e))
                       (origlib (caddr e)))
                   (list sym sym origlib)))
               exports))))

    ;; Mark as top-level for (yunifake program)
    (cond
      ((equal? '(yunifake program) libname)
       (set! has-macro-export? #t)))

    ;; Register a library
    (mark-as-loaded! libname pth sequence has-macro-export? exports imports))

  (define (load-library-file! path)
    (define content (file->sexp-list path))
    (PCK 'PARSING: path)
    (cond
      ((and (pair? content) (pair? (car content)))
       (let ((lib (car content)))
        (let ((libhead (car lib))
              (libname (cadr lib))
              (export-clauses (caddr lib))
              (import-clauses (cadddr lib))
              (sequence (cddddr lib)))
          (cond
            ((not (eq? 'library libhead))
             (error "load-library-file: Malformed library" 
                    libhead)))
          (cond
            ((not (eq? 'import (car import-clauses)))
             (error "load-library-file: Malformed import caluse" 
                    import-clauses)))
          (cond
            ((not (eq? 'export (car export-clauses)))
             (error "load-library-file: Malformed export caluse" 
                    export-clauses)))

          (process-sequence! path
                             libname 
                             (cdr import-clauses) 
                             (cdr export-clauses)
                             sequence))))
      (else
        (error "load-library-file: Malformed library" path))))

  (parseargs! input-args*)

  ;; Set program-file (the first argument)
  (cond
    ((pair? arg*)
     (let ((prog (car arg*))
           (arg (cdr arg*)))
       (set! program-file prog)
       (set! arg* arg)))
    (else
      (error "run: Program-file not found")))

  (PCK 'RUN: program-file)

  (cond ((not (and (string? program-file) (file-exists? program-file)))
         (error "run: File not found" program-file)))

  ;; Lookup for import clause
  (let ((code (file->sexp-list program-file)))
   (let ((import-clauses (car code))
         (sequence (cdr code)))
     (cond
       ((not (eq? 'import (car import-clauses)))
        (error "Malformed program" program-file) ))
     (process-sequence! program-file
                        '(yunifake program) ;; Special name
                        (cdr import-clauses)
                        '()
                        sequence)))

  (run))

