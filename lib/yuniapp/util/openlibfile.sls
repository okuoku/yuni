(library (yuniapp util openlibfile)
         (export openlibfile)
         (import (yuni scheme)
                 (yuni compat file-ops)) 
         
;;          

(define (libname->filename libname)
  (define (itr acc cur)
    (if (pair? cur)
      (let ((a (car cur)))
       (itr (string-append acc "/" (symbol->string a)) (cdr cur)))
      acc) )
  (itr "" libname))

(define (mkdirp-1! prefix libname)
  (unless (= 1 (length libname))
    (let* ((dirname (car libname))
           (pth (string-append prefix "/" (symbol->string dirname))))
      (unless (file-directory? pth)
        (create-directory pth))
      (mkdirp-1! pth (cdr libname)))))

(define (openlibfile output? libname prefix suffix) ;; => port / #f
  (when output?
    (mkdirp-1! prefix libname))
  (let* ((f (libname->filename libname))
         (fn (string-append prefix "/" f suffix)))
    (cond
      (output?
        (when (file-exists? fn)
          (delete-file fn))
        (open-output-file fn))
      (else
        (and (file-exists? fn)
             (open-input-file fn))))))
         
)
