(library (chibi-yuni compat file-ops)
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
                 (chibi filesystem))


(define directory-list directory-files)

(define (system-msdos-style-path?)
  ;; FIXME
  #f)

)


