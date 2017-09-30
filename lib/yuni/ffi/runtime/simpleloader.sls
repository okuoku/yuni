(library (yuni ffi runtime simpleloader)
         (export make-simpleloader)
         (import (yuni scheme)
                 (yuniconfig build))

(define prefix+suffix*
  (let ((p (yuniconfig-platform)))
   (cond
     ((or (string=? "WIN32" p) (string=? "WIN64" p))
      '(("" . "dll")))
     (else
       '(("lib" . "so")   ;; Linux/BSD (ELF)
         ("lib" . "dyld") ;; OSX MACH-O
         ("cyg" . "dll"))))))

(define (try-transforms proc base str)
  (define (itr cur)
    (and (pair? cur)
         (let* ((a (car cur))
                (d (cdr cur))
                (path (string-append base "/" (car a) str "." (cdr a))))
           (display (list 'TRY: path)) (newline)
           ;; FIXME: Use path-append to support UNC paths on Win32
           (or (proc path)
               (itr d)))))
  (itr prefix+suffix*))

(define (try-paths proc str paths)
  (define (itr cur)
    (and (pair? cur)
         (let ((a (car cur))
               (d (cdr cur)))
           ;; FIXME: Use path-append to support UNC paths on Win32
           (or (proc a)
               (itr d)))))
  (itr paths))

(define (make-simpleloader module-path module-loader)
  (define (simpleloader name)
    (let ((path* (module-path)))
     (try-paths (lambda (p) (try-transforms module-loader p name))
                name
                path*)))
  simpleloader)
         
)
