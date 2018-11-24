;;
;; yuni library manager: Core
;; 

; Globals

;; List of loaded libraries
(define *yuni/libraries* '())

; Library object
;;
;; LIB := #(<LIBNAME> <REALIZED?> <PROMOTED?>
;;          <IMPORT-LIBS> <RENAMEPAIRS>)
;; RENAMEPAIRS := ((source-name . global-sym) ...)
;; 

(define (yuni/library-name lib) (vector-ref lib 0))
;(define (yuni/library-realized? lib) (vector-ref lib 1))
;(define (yuni/library-realized?-set! lib x) (vector-set! lib 1 x))
(define (yuni/library-promoted? lib) (vector-ref lib 2))
(define (yuni/library-promoted?-set! lib x) (vector-set! lib 2 x))
(define (yuni/library-import-lib* lib) (vector-ref lib 3))
(define (yuni/library-renamepair* lib) (vector-ref lib 4))

(define (yuni/register-library! libname renames)
  (let ((newlib (make-vector 5 #f)))
   (vector-set! newlib 0 libname)
   (vector-set! newlib 1 #t)
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
(define (yuni/library-lookup-itr cur nam)
  (and (pair? cur)
       (if (equal? (yuni/library-name (car cur)) nam)
         (car cur)
         (yuni/library-lookup-itr (cdr cur) nam))))

(define (yuni/library-lookup nam)
  (yuni/library-lookup-itr *yuni/libraries* nam))


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
             (setters (map (lambda (e) (list 'set! (cdr e) (car e))) renames)))
        (PCK 'RENAMING: libname renames)

        (cons 'begin
              (list (yuni/xform-realize-library-hook-itr '() import* #f)
                    (cons 'begin storages)
                    (cons 'let 
                          (cons '() (list (cons 'begin libbody)
                                          (cons 'begin setters))))
                    (list 'yuni/register-library! (list 'quote libname)
                          (list 'quote renames))))
        
        ))))

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
     ((not (yuni/library-promoted? lib))
      (PCK 'DO-PROMOTE: lib)
      (cond
        ;; Set promoted? flag if we are expanding into global context
        (promote? (yuni/library-promoted?-set! lib #t)))
      (yuni/xform-promote-library lib))
     (else 
       ;; Do nothing
       #t))))

