(library (yuni minife libfilepath)
         (export make-libfilepath
                 library-name->path)
         (import (yuni scheme))

;; Library file manager (took from yuniloader)

(define ERRPORT current-error-port)
(define (PCK . obj)
  (if #t  ;; %verbose
    (begin
      (if #t ;; (not DEBUGGING)
        (begin
          (display "-> " (ERRPORT))
          (for-each (lambda (e)
                      (write e (ERRPORT))
                      (display " " (ERRPORT)))
                    obj)
          (newline (ERRPORT)))))))

(define (make-libfilepath import-dirs extensions)

  (define (make-library-path base nam ext)
    (if (pair? nam)
      (make-library-path (string-append (string-append base "/")
                                        (symbol->string (car nam)))
                         (cdr nam)
                         ext)
      (string-append base "." ext)))

  (define (library-name->path name)
    (define (itr0 base rest)
      (and (pair? rest)
           (or (let ((name (make-library-path base name (car rest))))
                (PCK 'TRYING: name)
                (and (file-exists? name)
                     name))
               (itr0 base (cdr rest)))))
    (define (itr rest)
      (and (pair? rest)
           (or (itr0 (car rest) extensions)
               (itr (cdr rest)))))
    (PCK 'LOOKUP: name)
    (itr import-dirs))

  library-name->path)

(define (library-name->path mgr name) (mgr name))

)
