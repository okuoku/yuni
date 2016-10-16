(library (gauche-yuni compat file-ops)
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
         (import (gauche base))

(define file-regular? file-is-regular?)
(define file-directory? file-is-directory?)

;; FIXME: it's mkdir -p
(define create-directory create-directory*)

;; FIXME: it's rm -r
(define delete-directory delete-directory*)


)


