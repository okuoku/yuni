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

(define-primitive-names/yunifake
  file-regular?
  file-directory?
  directory-list
  current-directory

  delete-directory
  create-directory
  )


)
