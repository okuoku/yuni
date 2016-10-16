(library (gambit-yuni compat file-ops)
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


(define (%%file-type-eq? pth sym)
  (let ((ifo (file-info pth)))
   (eq? sym (file-info-type ifo))))

(define (file-regular? pth)
  (%%file-type-eq? pth 'regular))

(define (file-directory? pth)
  (%%file-type-eq? pth 'directory))

(define (directory-list pth)
  (let ((p (open-directory pth)))
   (read-all p)))

)


