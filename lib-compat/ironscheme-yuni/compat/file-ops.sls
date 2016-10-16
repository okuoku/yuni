(library (ironscheme-yuni compat file-ops)
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
         (import (rnrs)
                 (ironscheme environment) ;; Current directory
                 (ironscheme files))


;;

(define (file-regular? pth)
  (error #f "Unimpl"))

(define (file-directory? pth)
  (error #f "Unimpl"))

(define (directory-list pth)
  (append
    (get-files pth)
    (get-directories pth)))

)


