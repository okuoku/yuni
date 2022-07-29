(library (racket-yuni compat file-ops)
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
         (import (yuni scheme)
                 (rename
                   (only (racket base)
                         directory-list
                         current-directory
                       
                         directory-exists?
                         make-directory
                         delete-directory
                         path->string
                         list->vector
                         map)
                   (map racket:map)
                   (list->vector racket:list->vector)
                   (directory-list racket:directory-list)))
         

(define (file-regular? x)
  (and (file-exists? x)
       (not (file-directory? x))))
         
(define (file-directory? x)
  (directory-exists? x))

(define (create-directory x)
  (make-directory x))

(define (directory-list x)
  (let* ((ml (racket:map (lambda (e) (path->string e)) 
                  (racket:directory-list x)))
         (mv (racket:list->vector ml)))
    (vector->list mv)))

)
