(library (mit-scheme-yuni compat file-ops)
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
         (import (yuni scheme))


(define current-directory working-directory-pathname)

;; FIXME: It returns absolute paths
(define direcrtory-list directory-read)
         
(define create-directory make-directory)

)


