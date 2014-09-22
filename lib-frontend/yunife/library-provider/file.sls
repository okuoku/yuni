(library (yunife library-provider file)
         (export make-file-library-provider)
         (import (yuni scheme)
                 (yunife logging))
;;

(define (make-library-path nam base ext)
  ;(PCK 'make-library-path: base nam)
  (if (pair? nam)
    (make-library-path (string-append (string-append base "/")
                                      (symbol->string (car nam)))
                       (cdr nam))
    (string-append base ext)))

(define (library-name->path import-dirs name ext)
  (define (itr rest)
    (if (pair? rest)
      (or (let ((name (make-library-path name (car rest) ext)))
           (PCK 'TRYING: name)
           (and (file-exists? name)
                name))
          (itr (cdr rest)))
      (error "library-name->path: Cannot find library for" name)))
  (PCK 'LOOKUP: name)
  (itr import-dirs))

(define (make-file-library-provider import-dirs ext)
  ;; FIXME: ext is not a list...
  (lambda (libname k)
    (k libname (library-name->path import-dirs libname ext))))
         
)
