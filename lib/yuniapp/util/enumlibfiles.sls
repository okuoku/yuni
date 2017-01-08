(library (yuniapp util enumlibfiles)
         (export enumlibfiles)
         (import (yuni scheme)
                 (yuni compat file-ops))

         
;; (enumlibfiles dir) => [file ...]

(define (enumlibfiles dir)
  (define acc '())
  (define (makepath dir name)
    (string-append dir "/" name))
  (define (add-file! file)
    (set! acc (cons file acc)))
  (define (dotname? str)
    (and (string? str) (char=? #\. (string-ref str 0))))
  (define (sls? str)
    (let ((len (string-length str)))
     (and (< 4 len)
          (let ((s (substring str (- len 4) len)))
           (string=? ".sls" s)))))

  (define (lis pth l)
    (and (pair? l)
         (let* ((c (car l))
                (p (makepath pth c)))
          (cond 
            ((dotname? c)
             'do-nothing)
            ((and (file-regular? p) (sls? c))
             (add-file! p))
            ((file-directory? p)
             (itr p)))
          (lis pth (cdr l)))))

  (define (itr pth)
    (let ((l (directory-list pth)))
     (lis pth l)))

  (itr dir)
  acc)
         
)
