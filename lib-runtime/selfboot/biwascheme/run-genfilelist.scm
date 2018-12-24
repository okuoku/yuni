(define (filelist->js-obj lis)
  (define (libname->string libname)
    (let loop ((q (cdr libname))
               (cur (symbol->string (car libname))))
      (if (null? q)
        cur
        (loop (cdr q)
              (string-append cur " " (symbol->string (car q)))))))
  (define (libname->array libname)
    (list->js-array (map symbol->string libname)))
  (list->js-array
    (map (lambda (e)
           (let ((libname (car e))
                 (dir (cadr e))
                 (pth (caddr e))
                 (a (cadddr e)))
             (js-obj "libname" (and libname (libname->array libname))
                     "dir" dir "pth" pth 
                     "alias"  (and a (libname->array a)))))
         lis)))

(define entrypoints* (js-array->list (yuni/js-import "entrypoints")))
(define loadpath* (js-array->list (yuni/js-import "loadpaths")))

(filelist->js-obj (%%selfboot-gen-filelist loadpath* entrypoints*))
