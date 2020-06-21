;;
;; yuni library manager: Core
;; 

; Globals

;; List of loaded libraries
(define *yuni/libraries* '())

; Library object
;;
;; LIB := #(<LIBNAME> <REALIZED?> <PROMOTED?>
;;          <IMPORT-LIBS> <RENAMEPAIRS> <ALIASES>)
;; RENAMEPAIRS := ((source-name . global-sym) ...)
;; 

(define (yuni/library-name lib) (vector-ref lib 0))
;(define (yuni/library-realized? lib) (vector-ref lib 1))
;(define (yuni/library-realized?-set! lib x) (vector-set! lib 1 x))
(define (yuni/library-promoted? lib) (vector-ref lib 2))
(define (yuni/library-promoted?-set! lib x) (vector-set! lib 2 x))
(define (yuni/library-import-lib* lib) (vector-ref lib 3))
(define (yuni/library-renamepair* lib) (vector-ref lib 4))
(define (yuni/library-aliasname* lib) (vector-ref lib 5))
(define (yuni/library-add-alias! lib name)
  (let ((x (yuni/library-aliasname* lib)))
   (vector-set! lib 5 (cons name x))))

(define *yuni/base-library*
  (vector 
    #f ;; name
    #t ;; realized?
    #t ;; promoted?
    '() ;; Import-libs
    '() ;; Rename-pairs
    '() ;; Aliases
    ))

(define (yuni/register-library! libname renames)
  (let ((newlib (make-vector 6 #f)))
   (vector-set! newlib 0 libname)
   (vector-set! newlib 1 #t)
   (vector-set! newlib 5 '())
   (cond
     (renames (vector-set! newlib 2 #f)
              (vector-set! newlib 4 renames))
     (else (vector-set! newlib 2 #t)
           (vector-set! newlib 4 #f))) 

   (PCK 'REGLIB! libname)
   (set! *yuni/libraries* (cons newlib *yuni/libraries*))))

(define (yuni/register-library-alias! from to)
  (let ((fromlib (yuni/library-lookup from)))
   ;; FIXME: Copy more
   (yuni/register-library! to (yuni/library-renamepair* fromlib))))

; Procedures
(define (yuni/library-lookup-check-alias orig q nam)
  (and (pair? q)
       (or (and (equal? (car q) nam) orig)
           (yuni/library-lookup-check-alias orig (cdr q) nam))))

(define (yuni/library-lookup-itr cur nam)
  (and (pair? cur)
       (or (yuni/library-lookup-check-alias 
             (car cur)
             (yuni/library-aliasname* (car cur)) nam)
           (if (equal? (yuni/library-name (car cur)) nam)
             (car cur)
             (yuni/library-lookup-itr (cdr cur) nam)))))

(define (yuni/library-check-core-libs-itr cur nam)
  (and (pair? cur)
       (let ((a (car cur))
             (d (cdr cur)))
         (or (and (equal? a nam) *yuni/base-library*)
             (yuni/library-check-core-libs-itr d nam)))))

(define (yuni/library-lookup nam)
  (PCK 'LOOKUP nam)
  ;; FIXME: Adjust location of %%selfboot-core-libs
  (or (yuni/library-check-core-libs-itr %%selfboot-core-libs nam)
      (yuni/library-lookup-itr *yuni/libraries* nam)))


(define (yuni/library-scan-sequence-itr var* stx* libbody) ;; => (var* . stx*)
  (cond
    ((pair? libbody)
     (let ((a (car libbody))
           (d (cdr libbody)))
       (define (skip) (yuni/library-scan-sequence-itr var* stx* d))
       (define (var x) (yuni/library-scan-sequence-itr (cons x var*) stx* d))
       (define (stx x) (yuni/library-scan-sequence-itr var* (cons x stx*) d))
       (define (cap proc x)
         (if (pair? x)
           (if (symbol? (car x)) (proc (car x)) (skip))
           (if (symbol? x) (proc x) (skip))))
       (cond ((pair? a)
              (case (car a)
                ((define) (cap var (cadr a)))
                ((define-syntax) (cap stx (cadr a)))
                ((begin) 
                 (let ((c (yuni/library-scan-sequence-itr var* stx* (cdr a))))
                  (yuni/library-scan-sequence-itr (car c) (cdr c) d)))
                (else (skip))))
             (else (skip)))))
    (else (cons var* stx*))))

;; Macro templates
(define (yuni/xform-promote-library lib)
  (cons 'begin (map (lambda (e) (list 'define (car e) (cdr e)))
                    (yuni/library-renamepair* lib))))

(define yuni/FIXME-DEFAULT-MACROS '(~ :=))
(define (yuni/xform-realize-library-hook-itr cur import* promote?) 
  ;; => (begin ...)
  (cond
    ((pair? import*)
     (yuni/xform-realize-library-hook-itr
       (cons (list 'yuni/realize-library-hook (car import*) promote?) cur)
       (cdr import*)
       promote?))
    (else (cons 'begin cur))))

(define (yuni/xform-library rename libname export* import* libbody)
  (define globals (yuni/library-scan-sequence-itr 
                    '() yuni/FIXME-DEFAULT-MACROS libbody))
  (define global-vars (car globals))
  (define global-stxs (cdr globals))
  (define promote-required? #f)
  (define (scan-stxs! cur)
    (cond
      ((and (not promote-required?) (pair? cur))
       (let loop ((c export*))
        (and (pair? c)
             (cond ((eq? (car c) (car cur))
                    (set! promote-required? #t) #f)
                   (else (loop (cdr c))))))
       (scan-stxs! (cdr cur)))
      (else #f)))
  (define (renamefilt e)
    (list (car e) (cdr e)))
  (define (calcinrenames renames import*)
    (let loop ((cur renames)
               (q import*))
      (if (pair? q)
        (let ((libname (car q))
              (d (cdr q)))
          (let* ((lib (yuni/library-lookup libname))
                 (needthis? (and lib (not (yuni/library-promoted? lib)))))
            (if needthis?
              (loop (append (map renamefilt (yuni/library-renamepair* lib)) cur) d)
              (loop cur d))))
        cur)))

  (scan-stxs! global-stxs)
  ;; FIXME: BiwaScheme requires every define-syntax top-level
  ;;        Enforce promote if the library defined any syntax
  (set! promote-required? (not (= (length global-stxs)
                                  (length yuni/FIXME-DEFAULT-MACROS))))

  (cond
    (promote-required?
      (PCK 'AUTOPROMOTE: libname)
      ;; Paste library body into top-level
      (cons 'begin
            (list (yuni/xform-realize-library-hook-itr '() import* #t)
                  (cons 'begin libbody)
                  (list 'yuni/register-library! (list 'quote libname)
                        #f))))
    (else 
      ;; Inject let-guard to protect global scope
      (let* ((renames (map (lambda (e) (cons e (rename e))) export*))
             (storages (map (lambda (e) (list 'define (cdr e) #f)) renames))
             (setters (map (lambda (e) (list 'set! (cdr e) (car e))) renames))
             (prefix (yuni/xform-realize-library-hook-itr '() import* #f))
             (inrenames (calcinrenames 
                          ;; Start with base-library import
                          (map renamefilt (yuni/library-renamepair* *yuni/base-library*)) 
                          import*))
             (code (cons 'let (cons inrenames 
                                    (append libbody
                                            (cons (cons 'begin setters)
                                                  '()))))))
        (PCK 'RENAMING: libname renames)
        ;(PCK 'CODE: code)
        (PCK 'INRENAMES: inrenames)

        (cons 'begin
              (list prefix
                    (cons 'begin storages)
                    code
                    (list 'yuni/register-library! (list 'quote libname)
                          (list 'quote renames))))))))

(define (yuni/xform-polyfill rename defsym exportsym body)
  (let ((mysym (rename exportsym)))
   (yuni/base-library-add-var! mysym exportsym)
   ;; FIXME: Expand qq
   `(begin 
      (define ,mysym #f)
      (let ()
        ,@body
        (set! ,mysym ,defsym)))))

(define (yuni/xform-top-level body)
  (define renames (map (lambda (p) (list (car p) (cdr p)))
                       (yuni/library-renamepair* *yuni/base-library*)))
  (let ((import-clause (car body))
        (realbody (cdr body)))
    ;; FIXME: Expand qq
    `(begin
       ,import-clause
       (let ,renames
        ,@realbody))))

;; Library runtimes
(define (yuni/realize-library! loadlib libname promote?) ;; => code
  (let ((lib (yuni/library-lookup libname)))
   (cond
     ((not lib) 
      (loadlib libname)
      (set! lib (yuni/library-lookup libname))))
   (PCK libname '=> lib)
   ;(yuni/library-realized?-set! lib #t)
   (cond
     ((and promote? (not (yuni/library-promoted? lib)))
      (PCK 'DO-PROMOTE: lib)
      ;; Set promoted? flag if we are expanding into global context
      (yuni/library-promoted?-set! lib #t)
      ;; Return promoted output
      (yuni/xform-promote-library lib))
     (else 
       ;; Do nothing
       #t))))

(define (yuni/base-library-add-var! sym varname)
  (PCK 'ADD-VAR: varname sym)
  (vector-set! 
    *yuni/base-library*
    4 
    (cons (cons varname sym) 
          (yuni/library-renamepair* *yuni/base-library*))))
