(library (yunife library-provider alias)
         (export make-alias-library-provider)
         (import (yuni scheme)
                 (yunife logging))

;;

(define (gen-target-name* alias* libname)
  (define (genname from)
    (cons from (cdr libname)))
  (define (itr cur rest)
    (if (pair? rest)
      ;; FIXME: support 2 or more deep alias
      (let ((from (caar rest))
            (to (cdar rest))
            (next (cdr rest)))
        (if (eq? (car libname) to)
          (itr (cons (genname from) cur) next)
          (itr cur next)))
      (reverse cur)))
  
  (itr '() alias*))

;; 
;; ALIAS SPEC FORMAT ::
;; 
;;   (nmosh-yuni . yuni)      ;; first element only
;;                            ;; (nmosh-yuni some thing) => (yuni some thing)
;; 

(define (make-alias-library-provider alias* provider)
  (lambda (libname k)
    (define (try l)
      (if (pair? l)
        ;; Try 
        (let ((n (provider (car l) (lambda (name obj) obj))))
         (if n 
           ;; Found.
           (k (car l) n)
           (try (cdr l))))
        ;; Not found. Fallback to default
        (provider libname k)))
    (let ((target-name* (gen-target-name* alias* libname)))
     (try target-name*)))) 
)
