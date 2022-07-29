(library (chicken-yuni compat file-ops)
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
                 (chicken file)
                 (chicken file posix)
                 (chicken process-context))

(define file-regular? regular-file?)

(define file-directory? directory?)

(define (directory-list pth)
  (glob (string-append pth "/*")))

)


