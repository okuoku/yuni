(library (yuni ffi scmtemplate root)
         (export
           get-scm-libname
           get-scm-libname/constants
           ;get-scm-libname/bridgestubs
           put-scm-stubsource/constants
           )
         (import (yuni scheme)
                 (yuni base match)
                 (yuni ffi ctemplate util)
                 (yuni ffi database root)
                 (yuni ffi database libinfo)
                 (yuni ffi database flatten))

;; Unlike C stubsource(yuni ffi ctemplate root),
;; SCM root would emit several actual libraries

(define (get-scm-libname db)
  (define libinfo (database-libinfo db))
  (define scmname (libinfo-scheme-name libinfo))
  (cons 'yuniffi scmname))

(define (get-c-libname db)
  (define libinfo (database-libinfo db))
  (define cname (libinfo-c-name libinfo))
  cname)
(define (get-c-dllname db)
  (string-append "yunistub_" (symbol->string (get-c-libname db))))

(define (append-libname lis sym)
  (define r (reverse lis))
  (let ((a (car r))
        (d (cdr r)))
    (reverse (cons (string->symbol
                     (string-append (symbol->string a)
                                    "-"
                                    (symbol->string sym)))
                   d))))

(define (get-scm-libname/constants db)
  (append-libname (get-scm-libname db) 'constants))

(define (get-scm-libname/bridgestubs db)
  (append-libname (get-scm-libname db) 'bridgestubs))

(define (emit-define-constants port libstate flatdb)
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define (one e)
    (match e
           (('constant label type value sizeof . attr)
            (p "(define-constant " label " " libstate " \"" 
               label "\" " type ")"))
           (('layout label . bogus)
            (p "(define-layout-constant " label " " libstate " \""
               label "\")"))
           (('aggregate-entry label . bogus)
            (p "(define-aggregate-entry-constant " label " " libstate " \""
               label "\")"))
           ))
  (define (itr cur)
    (when (pair? cur)
      (one (car cur))
      (itr (cdr cur))))
  (itr flatdb))

(define (emit-bridgestub-constants port libstate flatdb)
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define (expand-stubtypes label)
    (define sufx1 '(forward-0 forward-1 backward-1 backward-2))
    (define sufx2 '("_forward0" "_forward1" "_backward1" "_backward2"))
    (apply 
      append 
      (map (lambda (sym str)
             (list
               "(" sym 
               " \"" (string-append "ystub_" label str) "\")")) 
           sufx1 sufx2)))
  (define (one e)
    (match e
           ((label . bogus)
            (let ((ex `("(define-bridgestub-constant "
                        ,label
                        " "
                        ,libstate
                        " "
                        ,@(expand-stubtypes label)
                        ")")))
              (apply p ex)))))
  (define (itr cur)
    (when (pair? cur)
      (one (car cur))
      (itr (cdr cur))))
  (itr flatdb))

(define (gen-syms/constants flatdb)
  (define (one e)
    (match e
           (('constant label . bogus)
            (string->symbol label))
           (('layout label . bogus)
            (string->symbol label))
           (('aggregate-entry label . bogus)
            (string->symbol label))))
  (define (itr cur acc)
    (if (pair? cur)
      (itr (cdr cur) (cons (one (car cur)) acc))
      (reverse acc)))
  (itr flatdb '()))

(define (gen-syms/bridgestubs flatdb)
  (define (one e)
    (match e
           ((label . bogus)
            (string->symbol label))))
  (define (itr cur acc)
    (if (pair? cur)
      (itr (cdr cur) (cons (one (car cur)) acc))
      (reverse acc)))
  (itr flatdb '()))

(define (put-scm-stubsource/constants port db)
  (define (p . obj) (apply put-obj port (append obj (list "\n"))))
  (define (dropname x)
    (or (and x
             (cadr x)) 
        '()))
  ;; Currently, we do merge these two. In future, we will split them because
  ;; constants part is ABI stable and can be cached.
  (define flat (dropname (database->flatten/constants db)))
  (define flat2 (dropname (database->flatten/functions db)))
  (define syms/constants (gen-syms/constants flat))
  (define syms/bridgestubs (gen-syms/bridgestubs flat2))
  (define syms (append syms/constants syms/bridgestubs))
  (define c-libname (get-c-libname db))
  (define c-dllname (get-c-dllname db))

  ; header
  (p "(library " (get-scm-libname/constants db))
  ; export
  (p "(export ")
  (for-each (lambda (sym) (p sym)) syms)
  (p ")")
  ; import
  (p "(import (yuni ffi runtime yunistub-abiv0))")
  ; body
  (p "(define-library-state *LIBSTATE* \"" c-dllname "\" \""
     c-libname "\")")
  (p)
  (emit-define-constants port "*LIBSTATE*" flat)
  (emit-bridgestub-constants port "*LIBSTATE*" flat2)
  (p ")"))

)
