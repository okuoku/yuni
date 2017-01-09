(library (guile-yuni compat file-ops)
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
                 (only (guile)
                       stat
                       stat:type
                       opendir
                       readdir
                       closedir
                       getcwd
                       mkdir
                       rmdir))

         
(define (%%file-type-eq? pth sym)
  (let ((st (stat pth)))
   (eq? sym (stat:type st))))

(define (file-regular? pth) (and (file-exists? pth) 
                                 (%%file-type-eq? pth 'regular)))
(define (file-directory? pth) (and (file-exists? pth) 
                                   (%%file-type-eq? pth 'directory)))

(define (directory-list pth)
  (define ret '())
  (let ((dir (opendir pth)))
   (let loop ()
    (let ((e (readdir dir)))
     (cond
       ((eof-object? e)
        (closedir dir))
       (else
         (set! ret (cons e ret))
         (loop)))) ))
  ret)


;; Process
(define current-directory getcwd)


(define create-directory mkdir)

(define delete-directory rmdir)

)
