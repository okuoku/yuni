(library (vicare-yuni compat file-ops)
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
                 (only (vicare posix)
                       file-is-regular-file?
                       file-is-directory?
                       
                       )) 

(define file-regular? file-is-regular-file?)
(define file-directory? file-is-directory?)

(define (directory-list pth)
  (define ret '())
  (let ((dir (opendir pth)))
   (let loop ()
    (let ((e (readdir/string dir)))
     (cond
       (e (set! ret (cons e ret))
          (loop))
       (else (closedir dir))))))
  ret)

(define current-directory getcwd/string)

(define (create-directory pth)
  ;; mkdir as 777
  (mkdir pth #x1ff))

(define delete-directory rmdir)

)


