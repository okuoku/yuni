;; Tentative library file manager
(library (yunife libmgr)
         (export
           make-libmgr
           libmgr-add-alias-map!
           libmgr-add-path!
           libmgr-resolve
           libmgr-read)
         (import (yuni scheme)
                 (yunife sourcereader)
                 (yunife debugging))

(define (make-libmgr)
  (define loadpath* '())
  (define alias-map* '())
  (define located* '())

  (define (add-alias-map! from to)
    (set! located* '())
    (set! alias-map* (cons (cons from to)
                           alias-map*)))
  (define (add-path! pth)
    (set! located* '())
    (set! loadpath* (cons pth loadpath*)))

  (define (explode-libname libname)
    (define out (list libname))
    (let ((f (car libname))
          (d (cdr libname)))
     (let loop ((a (reverse alias-map*)))
      ;(PCK 'Loop: a)
      (cond
        ((null? a) out)
        ((eq? (caar a) f)
         (set! out (cons (cons (cdar a) d) out))
         (loop (cdr a)))
        (else (loop (cdr a)))))))

  (define (make-library-path base nam)
    (if (pair? nam)
        (make-library-path (string-append base "/" (symbol->string (car nam)))
                           (cdr nam))
        (string-append base ".sls")))

  (define (file-exists?0 pth)
    ;(PCK 'Trying pth)
    (file-exists? pth))

  (define (locate-one! libname) ;; => #f / libpath
    (let loop ((m loadpath*))
     (and (pair? m)
          (or (let ((pth (make-library-path (car m) libname)))
               (and (file-exists?0 pth) pth))
              (loop (cdr m))))))

  (define (locate! libname)
    ;(PCK 'Locate: libname)
    (let loop ((libnames (explode-libname libname)))
     (and (pair? libnames)
          (or (locate-one! (car libnames))
              (loop (cdr libnames))))))

  (define (do-read libname)
    (let ((pth (locate! libname)))
     (and pth
          (let ((src (read-source pth)))
           (unless (= 1 (length src))
             (error "Malformed library" pth))
           (car src))))) 

  (define (do-resolve libname)
    (locate! libname))

  (define (action sym . arg*)
    (case sym
      ((add-alias-map!) (apply add-alias-map! arg*))
      ((add-path!) (apply add-path! arg*))
      ((resolve) (apply do-resolve arg*))
      ((read) (apply do-read arg*))
      (else (error "Unknown op" sym))))

  action)

(define (libmgr-add-alias-map! mgr from to) (mgr 'add-alias-map! from to))
(define (libmgr-add-path! mgr pth) (mgr 'add-path! pth))
(define (libmgr-resolve mgr libname) (mgr 'resolve libname))
(define (libmgr-read mgr libname) (mgr 'read libname))

)
