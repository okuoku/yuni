;; STUB
(library (scm-yuni compat file-ops)
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


(define (file-regular? pth)
  ;; FIXME: how about non-directory special files..?
  (and (file-exists? pth)
       (let ((d (opendir pth)))
        (cond
          (d (closedir d) #f)
          (else #t)))))

(define (file-directory? pth)
  (and (file-exists? pth)
       (let ((d (opendir pth)))
        (cond
          (d (closedir d) #t)
          (else #f)))))

(define (current-directory) (getcwd))

(define (directory-list pth)
  (define lis* '())
  (let ((d (opendir pth)))
   (cond
     (d (let loop ((lis* '())
                   (n (readdir d)))
          (cond
            (n (loop (cons n lis*) (readdir d)))
            (else
              (closedir d)
              (reverse lis*)))))
     (else #f))))

(define (create-directory pth)
  (mkdir pth))

(define (delete-directory pth)
  (rmdir pth))

)
