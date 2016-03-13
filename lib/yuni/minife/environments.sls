(library (yuni minife environments)
         (export
           envframe-decl-binding!
           envframe-def-binding!
           envframe-add-binding!
           envframe-only-bindings!
           envframe-except-bindings!
           envframe-prefix-bindings!
           envframe-rename-bindings!
           envframe-lookup
           envframe-lookup-decl
           envframe-lookup-def
           envframe-import!
           envframe-new
           
           env-lookup
           env-current-frame
           env-current-add-unknown!
           env-up/merge!
           env-new-envframe!
           env-new)
         (import (yuni scheme)
                 (yuni minife identifiers))

(define (%envframe-for-each proc envframe)
  (for-each (lambda (sym+id) (proc sym+id))
            (car envframe)))

(define (%envframe-filter-sym! envframe proc)
  (define out '())
  (let ((c (car envframe)))
   (for-each (lambda (sym+id)
               (let ((sym (car sym+id))
                     (id (cdr sym+id)))
                 (let ((x (proc sym)))
                  (when x
                    (set! out (cons (cons (if (eq? x #t) sym x) id)
                                    out))))))
             c)
   (set-car! envframe (reverse out))))         

(define (envframe-only-bindings! envframe . sym)
  (define (filt s)
    (and (memv s sym) #t))
  (%envframe-filter-sym! envframe filt))

(define (envframe-except-bindings! envframe . sym)
  (define (filt s)
    (not (memv s sym)))
  (%envframe-filter-sym! envframe filt))

(define (envframe-prefix-bindings! envframe pref)
  (define name (symbol->string pref))
  (define (filt s)
    (string->symbol (string-append name (symbol->string s))))
  (%envframe-filter-sym! envframe filt))

(define (envframe-rename-bindings! envframe . ren)
  (define (filt s)
    (define (itr rest)
      (and (pair? rest)
           (let ((from (car (car rest)))
                 (to (cadr (car rest))))
             (or (and (eq? from s) to)
                 (itr (cdr rest))))))
    (or (itr ren)
        s))
  (%envframe-filter-sym! envframe filt))

(define (envframe-decl-binding! envframe sym id)
  (let ((c (car envframe)))
   (set-car! envframe (cons (cons (cons 'decl sym) id) c))))

(define (envframe-def-binding! envframe sym id)
  (let ((c (car envframe)))
   (set-car! envframe (cons (cons (cons 'def sym) id) c))))

(define (envframe-add-binding! envframe sym id)
  (let ((c (car envframe)))
   (set-car! envframe (cons (cons sym id) c))))

(define (%envframe-lookup-tagged envframe tag sym)
  (define (search rest)
    (and (pair? rest)
         (let ((p (caar rest)))
          (or (and (pair? p) 
                   (eq? (car p) tag)
                   (eq? (cdr p) sym)
                   (cdar rest))
              (search (cdr rest))))))
  (let ((l (car envframe)))
   (search l)))

(define (envframe-lookup-decl envframe sym)
  (%envframe-lookup-tagged envframe 'decl sym))

(define (envframe-lookup-def envframe sym)
  (%envframe-lookup-tagged envframe 'def sym))

(define (envframe-lookup envframe sym)
  (define (search rest)
    (and (pair? rest)
         (or (and (eq? (caar rest) sym)
                  (cdar rest))
             (search (cdr rest)))))
  (let ((l (car envframe)))
   (search l)))

(define (envframe-import! envframe libframe)
  (define (importone e)
    (let ((sym (car e))
          (id (cdr e)))
      (let ((origid (envframe-lookup envframe sym)))
       (cond
         (origid
           (unless (and (eq? (id-source-name origid) (id-source-name id))
                        (eq? (id-global-name origid) (id-global-name id))
                        (equal? (id-library origid) (id-library id)))
             (error "Unmatched import" origid id)))
         (else
           (envframe-add-binding! envframe sym id))))))
  (let ((l (car libframe)))
   (for-each importone l)))

(define (envframe-new)
  (cons '() #f))

(define (%env-content env) (car env))
(define (%env-content-set! env e) (set-car! env e))
(define (%env-capsule l) (cons l #f))

(define (env-lookup env sym)
  (define (search rest)
    (and (pair? rest)
         (or (envframe-lookup (env-current-frame (%env-capsule rest)) sym)
             (search (cdr rest)))))
  (search (%env-content env)))

(define (env-current-frame env)
  (caar (%env-content env)))

(define (env-current-add-unknown! env pair)
  (let ((p (car (%env-content env))))
   (let ((current-unknown (cdr p)))
    (set-cdr! p (cons pair current-unknown)))))

(define (%env-current-unknown-reset! env)
  (let ((p (car (%env-content env))))
   (set-cdr! p '())))

(define (env-up/merge! env)
  (let ((unknowns (cdar (cdr (%env-content env))))
        (cf (env-current-frame env))
        (next (cdr (%env-content env))))
    (let ((next-env (%env-capsule next)))
     ;; Activate current definitions
     (%envframe-for-each 
       (lambda (x+id)
         (let ((x (car x+id))
               (id (cdr x+id)))
           (when (pair? x)
             (let ((disp (car x))
                   (sym (cdr x)))
               (when (eq? disp 'def)
                 (envframe-add-binding! (env-current-frame next-env)
                                        sym id))))))
       cf)
     ;; Reset current unknowns
     (%env-current-unknown-reset! next-env)
     ;; Merge current-frame unknowns
     (for-each (lambda (e) 
                 (let ((id (env-lookup next-env (car e))))
                  (cond
                    (id 
                      (unless (id-variable? id)
                        (error "Invalid def match" e))
                      (set-car! e (id-global-name id)))
                    (else
                      (env-current-add-unknown! next-env e) ))))
               unknowns))
    (%env-content-set! env next)))

(define (env-new-envframe! env)
  (let ((c (%env-content env)))
   (%env-content-set! env (cons (cons (envframe-new) '()) c))))

(define (env-new)
  (%env-capsule (list (cons (envframe-new) '()))))

)
