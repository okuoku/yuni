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

           ;; ancient
           system-msdos-style-path?
           )
         (import (scheme base)
                 (primitives 
                   ;; Some of them are not described in the manual
                   list-directory
                   file-directory?
                   current-directory

                   ;; For create-directory
                   system

                   ;; For system-msdos-style-path?
                   system-features
                   )
                 )


(define directory-list list-directory)
(define (file-regular? x) 
  ;; FIXME: Does file-attributes working on Win32??
  (not (file-directory? x)))

(define (system-msdos-style-path?)
  (equal? (cdr (assq 'os-name (system-features)))))

(define (create-directory pth)
  ;; FIXME: Seriously??
  (system (string-append "mkdir " pth)))

(define (delete-directory pth)
  ;; FIXME: Seriously??
  (system (string-append "rmdir " pth)))

)


