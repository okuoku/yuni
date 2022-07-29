(library (digamma-yuni compat file-ops)
         (export 
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; mosh directory procedure
           create-directory
           delete-directory)
         (import (yuni scheme)
                 (only (core) 
                       create-directory
                       file-regular?
                       file-directory?)
                 (core files))

;; Digamma uses remove(3) for `delete-file` which is also applicable on dirs
(define delete-directory delete-file)

)
