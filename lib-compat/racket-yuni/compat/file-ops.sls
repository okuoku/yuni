(library (racket-yuni compat file-ops)
         (export
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; mosh directory procedure
           create-directory
           delete-directory

           ;; ancient
           system-msdos-style-path?
           )
         (import (yuni scheme)
                 (only (racket)
                       directory-list
                       current-directory
                       
                       directory-exists?
                       make-directory
                       delete-directory))
         

(define (file-regular? x)
  (and (file-exists? x)
       (not (file-directory? x))))
         
(define (file-directory? x)
  (directory-exists? x))

(define (create-directory x)
  (make-directory x))

(define (system-msdos-style-path?)
  ;; FIXME
  #f)

)
