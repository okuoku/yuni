(library (sagittarius-yuni compat file-ops)
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
         (import (sagittarius) (rnrs))

(define (directory-list x) (read-directory x))

)


