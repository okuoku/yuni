(library (chibi-scheme-yuni compat file-ops)
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
                 (chibi filesystem))


;; Cyclone uses remove(3) to delete-file
(define delete-directory delete-file)

;;
(define create-directory "UNIMPL")
(define file-regular? "UNIMPL")
(define file-directory? "UNIMPL")
(define current-directory "UNIMPL")

)


