(library (s7-yuni compat file-ops)
         (export
           ;; chez like file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory

           ;; mosh directory procedure
           create-directory
           delete-directory)
         (import (yuni scheme) ;; not really
                 )

(define (file-regular? x) (not (file-directory? x)))
(define (directory-list x) (directory->list x))
(define (file-directory? x) (directory? x))
;; FIXME: Insecure
(define (create-directory x) (system (format #f "mkdir ~A" x)))
(define (delete-directory x) (system (format #f "rmdir ~A" x)))
(define (current-directory) #f)
)
