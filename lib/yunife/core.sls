(library (yunife core)
         (export
           make-yunife
           yunife-add-path!
           yunife-add-alias-map!
           ;; Libname #t used for program
           yunife-load!
           yunife-load-sexp-list!
           yunife-get-library-code
           yunife-get-library-macro
           yunife-get-library-imports
           yunife-get-library-exports
           yunife-get-libsym
           yunife-add-primitives!)
         (import (yuni scheme)
                 (yunife libmgr)
                 (yuni lighteval)
                 (yuni hashtables)
                 (yunife sourcereader)
                 (yunife runtime synrules)
                 (yunife debugging)
                 (yunife core-transformers))

;;

(define (defmacro->xformer le code)
  ;; code = (define-macro (NAME . arg) ...)
  (let ((frm (cdr (cadr code)))
        (body (cddr code)))
    (let ((cls (lighteval-bind le `(lambda ,frm ,@body))))
     cls)))

(define (make-yunife)
  (define libname* '())
  (define ht-libcodes (make-symbol-hashtable))
  (define ht-libmacros (make-symbol-hashtable))
  (define ht-libimports (make-symbol-hashtable))
  (define ht-libexports (make-symbol-hashtable))

  (define libmgr (make-libmgr))
  (define le (make-lighteval-env))

  ;; Macro-Environment
  ;;  (<LIBNAME-SYM> . #f) -- primitive
  ;;  (<LIBNAME-SYM> . (<CODE> . <SOURCE>))
  (define (env-lookup env sym)
    (let ((c (hashtable-ref env sym #f)))
     (and c (cdr c))))

  (define (env-extend! env libname-sym name obj)
    (let ((p (hashtable-ref env name #f)))
     (cond
       (p (unless (eq? libname-sym (car p))
            (error "Macro dupe." p libname-sym)) )
       (else
         ;(PCK 'Register-Macro: name obj)
         (hashtable-set! env name (cons libname-sym obj))))))

  (define (env-import! env libname-sym)
    (let ((l (hashtable-ref ht-libmacros libname-sym #f)))
     (unless l
       (error "library is not imported yet" libname-sym))
     ;; FIXME: prepare hashtable-for-each
     (cond
       ((null? l)
        'ok)
       (else
         (let ((v (hashtable-keys l)))
          (vector-for-each
            (lambda (nam) 
              (let ((m (hashtable-ref l nam #f)))
               (env-extend! env (car m) nam (cdr m))))
            v))))))

  (define (make-env)
    (make-symbol-hashtable))

  ;; Expander 
  (define *unspecified* (cons #f #f))
  (define (expand-form! env libname-sym top-level? sexp cb)
    (define (pass)
      (cond
        ((pair? sexp)
         (let ((a *unspecified*)
               (d *unspecified*))
           (expand-form! env libname-sym #f (car sexp) 
                         (lambda (obj) (set! a obj)))
           (expand-form! env libname-sym #f (cdr sexp) 
                         (lambda (obj) (set! d obj)))
           (cond
             ((and top-level? (eq? a *unspecified*))
               ;; Allow car-part as syntax-definition
               (unless (or (eq? d *unspecified*) (null? d))
                 (error "non-terminate?" d)))
             (else
               (when (or (eq? a *unspecified*)
                         (eq? d *unspecified*))
                 (PCK 'Pass: sexp)
                 (error "???" (cons a d)))
               (cb (cons a d))))))
        (else (cb sexp))))
    (cond
      ((and (pair? sexp) (symbol? (car sexp)))
       (let ((head (car sexp)))
        (case head
          ((begin)
           (cond
             (top-level?
               ;; Shortcut
               (expand-sequence! env libname-sym #t (cdr sexp) cb))
             (else 
               ;(PCK 'Pass3: sexp)
               (pass))))
          ((quote) (cb sexp))
          ((define-macro)
           (unless top-level?
             (error "Non-top-level define-macro" sexp))
           (let ((seq (cdr sexp))
                 (name (caadr sexp)))
             (expand-form! env libname-sym #f 
                           seq
                           (lambda (code0)
                             (let* ((code (cons 'define-macro 
                                                code0))
                                    (func (defmacro->xformer le code)))
                               (env-extend! 
                                 env
                                 libname-sym
                                 name
                                 (cons func code)))))))
          (else
            (let ((mac (env-lookup env head)))
             (cond
               ((and mac (procedure? (car mac)))
                (let ((next (apply (car mac) (cdr sexp))))
                 ;(PCK '... next)
                 (expand-form! env libname-sym top-level? next cb)))
               (else (pass))))))))
      (else (pass))))

  (define (expand-sequence! env libname-sym top-level? sexp cb)
    (for-each (lambda (frm)
                (cond
                  ((pair? frm)
                   (expand-form! env libname-sym top-level? frm cb))
                  (else (cb frm))))
              sexp))

  ;; Library handlers
  (define (csafe str) ;; => string
    (define (safechar? c)
      (let ((code (char->integer c)))
        ;; Assumes ascii
        (or (<= 48 code 57) ;; 0 .. 9
            (<= 65 code 90) ;; A-Z and a-z
            (<= 97 code 122))))
    (let loop ((acc "")
               (q (string->list str)))
      (if (pair? q)
          (loop (string-append acc (if (safechar? (car q))
                                       (list->string (list (car q)))
                                       (string-append 
                                         "_"
                                         (number->string
                                           (char->integer (car q))))))
                (cdr q))
          acc)))

  (define (libname->symbol libname)
    (cond
      ((eq? #t libname) '**program**)
      (else
        (let loop ((str (csafe (symbol->string (car libname))))
                   (rest (cdr libname)))
         (if (pair? rest)
             (loop (string-append str "__" (symbol->string (car rest)))
                   (cdr rest))
             (string->symbol str))))))

  (define (ensure-library-loaded! libname)
    (let ((sym (libname->symbol libname)))
     (let ((code (hashtable-ref ht-libcodes sym #f))
           (macro (hashtable-ref ht-libcodes sym #f)))
       (unless (or code macro)
         (do-load/name! sym (libmgr-resolve libmgr libname))))))

  (define (process-import1! env clause)
    (unless (pair? clause)
      (error "Malformed import clause" clause))
    (let ((a (car clause)))
     (case a
       ((only rename except)
        ;; FIXME: Ignore spec here.
        (process-import1! env (cadr clause)))
       (else
         (ensure-library-loaded! clause)
         (env-import! env (libname->symbol clause))))))

  (define (process-import! env sexp)
    (for-each (lambda (c) (process-import1! env c))
              sexp))

  (define (process-toplevel! env libname-sym sexp) ;; => expanded
    (define src '())
    (define (cb frm) (set! src (cons frm src)))
    (expand-sequence! env libname-sym #t sexp cb)
    (reverse src))

  (define (do-load-library! libname-sym/name sexp)
    (let ((libname (cadr sexp))
          (export? (caddr sexp))
          (import? (cadddr sexp))
          (prog* (cddddr sexp)))
      (unless (eq? 'export (car export?))
        (error "Malformed export" export?))
      (unless (eq? 'import (car import?))
        (error "Malformed import" import?))
      (let ((imports (cdr import?))
            (exports (cdr export?))
            ;; Handle alias library name
            (libname-sym (or libname-sym/name (libname->symbol libname)))
            (env (make-env)))
        (process-import! env imports)
        (let* ((src (process-toplevel! env libname-sym prog*)))
          ;; Register library
          (hashtable-set! ht-libimports libname-sym imports)
          (hashtable-set! ht-libexports libname-sym exports)
          (hashtable-set! ht-libcodes libname-sym src)
          (hashtable-set! ht-libmacros libname-sym env)))))

  (define (do-load-program! sexp)
    (let ((import? (car sexp))
          (seq (cdr sexp)))
      (let ((libname-sym (libname->symbol #t))
            (env (make-env)))
        (unless (eq? 'import (car import?))
          (error "Malformed program" sexp))
        (process-import! env (cdr import?))
        (let ((src (process-toplevel! env libname-sym seq)))
         ;; Register library
         (hashtable-set! ht-libimports libname-sym (cdr import?))
         (hashtable-set! ht-libexports libname-sym '())
         (hashtable-set! ht-libcodes libname-sym src)
         (hashtable-set! ht-libmacros libname-sym env)))))

  (define (do-load-source! libname-sym code)
    (cond
      ((and (pair? code)
            (pair? (car code))
            (eq? 'library (caar code)))
       (do-load-library! libname-sym (car code))
       (do-load-source! libname-sym (cdr code)))
      ((null? code)
       'ok)
      (else
        (do-load-program! code))))

  (define (do-load-sexp-list! sexp)
    (do-load-source! #f sexp))

  (define (do-load! pth)
    (do-load/name! #f pth))

  (define (do-load/name! libname-sym pth)
    (let ((code (read-source pth)))
     (do-load-source! libname-sym code)))

  (define (macro-ht->alist ht)
    ;; FIXME: Implement it
    (let ((keys (hashtable-keys ht)))
     (map (lambda (k)
            (cons k (hashtable-ref ht k #f)))
          (vector->list keys))))

  (define (get-library-code libsym)
    (hashtable-ref ht-libcodes libsym #f))
  (define (get-library-macro libsym)
    (macro-ht->alist (hashtable-ref ht-libmacros libsym #f)))
  (define (get-library-imports libsym)
    (hashtable-ref ht-libimports libsym #f))
  (define (get-library-exports libsym)
    (hashtable-ref ht-libexports libsym #f))

  (define (add-primitives! libname prims*)
    (let ((sym (libname->symbol libname)))
     (hashtable-set! ht-libcodes sym '())
     (hashtable-set! ht-libmacros sym (make-primitive-macros prims*))))
  
  (define (action op . args)
    (case op
      ;; Actions
      ((load!) (apply do-load! args))
      ((load-sexp-list!) (apply do-load-sexp-list! args))
      ((add-primitives!) (apply add-primitives! args))
      ;; Proxy
      ((add-path!) (apply libmgr-add-path! libmgr args))
      ((add-alias-map!) (apply libmgr-add-alias-map! libmgr args))
      ;; query
      ((get-libsym) (apply libname->symbol args))
      ((get-library-code) (apply get-library-code args))
      ((get-library-macro) (apply get-library-macro args))
      ((get-library-imports) (apply get-library-imports args))
      ((get-library-exports) (apply get-library-exports args))
      (else
        (error "Invalid op" op))))

  (define (make-primitive-macros l)
    (define ht (make-symbol-hashtable))
    (for-each (lambda (p)
                (let ((name (car p))
                      (code+source (cdr p)))
                  (hashtable-set! ht name (cons '**CORE** code+source))))
              l)
    ht)

  ;; Init evaluator
  (lighteval-env-set! le 'yuni/gensym yuni/gensym)
  (lighteval-env-set! le 'yuni/make-synrule-baselib yuni/make-synrule-baselib)
  (lighteval-env-set! le 'yuni/synrule-compare yuni/synrule-compare)
  (lighteval-env-set! le 'yuni/cons-source yuni/cons-source)

  ;; Inject dummy library
  (add-primitives! '(yunivm-core-syntax)
                   ;; Compiler primitives
                   `((begin        . #f)
                     (if           . #f)
                     ;(when         . #f)
                     (lambda       . #f)
                     (set!         . #f)
                     (quote        . #f)
                     ($define/core . #f)
                     (letrec*      . #f)
                     ;; Aux syntax
                     (syntax-rules . #f)
                     (...          . #f)
                     (=>           . #f)
                     (_            . #f)
                     (else         . #f)
                     ;; (embedded) Macro
                     (define-syntax ,define-syntax/macro DUMMY)))
  ;;
  action)         

;; actions
(define (yunife-load! fe path) (fe 'load! path))
(define (yunife-load-sexp-list! fe sexp) (fe 'load-sexp-list! sexp))
(define (yunife-add-primitives! fe libname a*) (fe 'add-primitives! libname a*))
;; proxy
(define (yunife-add-path! fe path) (fe 'add-path! path))
(define (yunife-add-alias-map! fe from to) (fe 'add-alias-map! from to))
;; query
(define (yunife-get-libsym fe libname) (fe 'get-libsym libname))
(define (yunife-get-library-code fe libsym) (fe 'get-library-code libsym))
(define (yunife-get-library-macro fe libsym) (fe 'get-library-macro libsym))
(define (yunife-get-library-imports fe libsym) (fe 'get-library-imports libsym))
(define (yunife-get-library-exports fe libsym) (fe 'get-library-exports libsym))
         
)
