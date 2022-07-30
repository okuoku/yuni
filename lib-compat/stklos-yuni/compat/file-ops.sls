;; STUB
(library (stklos-yuni compat file-ops)
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

(define file-regular? "UNIMPL")
(define file-directory? "UNIMPL")
(define directory-list "UNIMPL")
(define current-directory "UNIMPL")
(define create-directory "UNIMPL")
(define delete-directory "UNIMPL")

)
