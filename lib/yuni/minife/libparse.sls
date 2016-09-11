(library (yuni minife libparse)
         (export
           libparse-libname
           libparse-import-sets
           libparse-export-specs
           libparse-body
           libparse-library?)
         (import (yuni scheme))

;;

(define (libparse-libname sexp)
  (and (libparse-library? sexp)
       (cadr sexp)))

(define (libparse-import-sets sexp)
  (define (return lis)
    (unless (eq? 'import (car lis))
      (error "malformed import set" lis))
    (cdr lis))
  (if (libparse-library? sexp)
    (return (car (cdr (cdr (cdr sexp)))))
    (return (car sexp))))

(define (libparse-export-specs sexp)
  (define (return lis)
    (unless (eq? 'export (car lis))
      (error "malformed export specs" lis))
    (cdr lis))
  (and (libparse-library? sexp)
    (return (car (cdr (cdr sexp))))))

(define (libparse-body sexp)
  (if (libparse-library? sexp)
    (cdr (cdr (cdr (cdr sexp))))
    (cdr sexp)))

(define (libparse-library? sexp)
  (and (pair? sexp) (eq? (car sexp) 'library)))
         
)
