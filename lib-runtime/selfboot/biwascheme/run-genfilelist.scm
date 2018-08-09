(define (gen-filelist loadpath* entrypoint*)
  (define deps* '())
  (define resolver
    (%selfboot-yuniconfig-gen-resolver 
      'biwascheme
      %selfboot-yuniroot))

  (define (libread libname)
    (write (list 'libread: libname))
    (newline)
    (let ((r (resolver libname)))
     (unless r
       (error "Cannot read" libname))
     (let ((orig (car r))
           (aliasname (cadr r))
           (dir (caddr r))
           (pth (cadddr r)))
       (car (%selfboot-file->sexp-list (string-append dir "/" pth))))))

  (define (libcheck0 libname)
    (let ((r (resolver libname)))
     (unless r
       (error "Cannot read" libname))
     (let ((orig (car r))
           (aliasname (cadr r))
           (dir (caddr r))
           (pth (cadddr r)))
       aliasname)))

  (define (libcheck libname)
    (write (list 'libcheck: libname))
    (newline)
    (cond
      ((equal? '(yuni scheme) libname) #f)
      (else (libcheck0 libname))))

  ;; Add loadpaths
  (for-each
    (lambda (path)
      (%selfboot-yuniconfig-resolver-add-loadpath!
        resolver
        path))
    loadpath*)

  ;; Collect deps
  (for-each
    (lambda (path)
      (let ((code (%selfboot-file->sexp-list path)))
       (set! deps* (append (%selfboot-program-depends code) deps*))))
    entrypoint*)

  ;; Generate liborder
  (let ((order (%selfboot-gen-loadorder libread libcheck deps*))
        (runtimefiles (%selfboot-yuniconfig-get-runtime-list 
                        'biwascheme %selfboot-yuniroot)))
   (append
     (map (lambda (path)
            (list #f %selfboot-yuniroot path #f))
          runtimefiles)
     ;; Resolve again..
     (map (lambda (names)
          (let* ((libname (car names))
                 (r (resolver libname)))
           (let ((dir (caddr r))
                 (pth (cadddr r)))
             (if (pair? (cdr names))
               (list libname dir pth (cadr names))
               (list libname dir pth #f)))))
        order))))

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

(filelist->js-obj (gen-filelist loadpath* entrypoints*))
