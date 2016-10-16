(library (larceny-yuni compat file-ops)
         (export 
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; mosh directory procedure
           create-directory
           delete-directory
           )
         (import (scheme base)
                 (primitives 
                   ;; Some of them are not described in the manual
                   list-directory
                   larceny:directory?
                   current-directory

                   ;; For create-directory
                   system))


(define directory-list list-directory)
(define file-directory? larceny:directory?)
(define (file-regular? x) 
  ;; FIXME: Does file-attributes working on Win32??
  (not (file-directory? x)))

(define (create-directory pth)
  ;; FIXME: Seriously??
  (system (string-append "mkdir " pth)))

(define (delete-directory pth)
  ;; FIXME: Seriously??
  (system (string-append "rmdir " pth)))

)

