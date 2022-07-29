(library (kawa-yuni compat file-ops)
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
                 (kawa lib ports)
                 (kawa lib files)
                 )


(define (file-regular? pth) (not (file-directory? pth)))

(define delete-directory delete-file)

;; FIXME: Undocumented?
(define directory-list directory-files)

(define current-directory current-path)

)


