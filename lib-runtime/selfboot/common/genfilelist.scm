;;
;; INPUTs:
;;  %%selfboot-program-args: (command-line) result
;;  %%selfboot-impl-type: `biwascheme` etc
;;  %%selfboot-yuniroot: yuniroot
;;  %%selfboot-core-libs: Ignored library names
;;

(define (%%selfboot-gen-filelist loadpath* entrypoint*)
  (define deps* '())
  (define (ignore-lib? lib)
    (define (itr rest)
      (and (pair? rest)
           (or (equal? (car rest) lib)
               (itr (cdr rest)))))
    (itr %%selfboot-core-libs))
  (define resolver
    (%selfboot-yuniconfig-gen-resolver
      %%selfboot-impl-type
      %%selfboot-yuniroot))

  (define (libread libname)
    ;(write (list 'libread: libname)) (newline)
    (let ((r (resolver libname)))
     (unless r
       (error "cannot read" libname))
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
    ;(write (list 'libcheck: libname)) (newline)
    (cond
      ((ignore-lib? libname) #f)
      (else (libcheck0 libname))))

  ;; Add loadpath
  (for-each
    (lambda (path)
      (%selfboot-yuniconfig-resolver-add-loadpath!
        resolver path))
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
                        %%selfboot-impl-type
                        %%selfboot-yuniroot)))
    (append
      (map (lambda (path)
             (cond
               ((symbol? path)
                ;; Polyfill
                (let ((actual-path (%%selfboot-yuniconfig-get-polyfill-path
                                     path)))
                  (list #f %%selfboot-yuniroot actual-path #f path)))
               (else
                 (list #f %%selfboot-yuniroot path #f (cons #f #f)))))
           runtimefiles)
      (map (lambda (libinfo)
             (let ((names (car libinfo))
                   (exports (cdr libinfo)))
              (let* ((libname (car names))
                     (r (resolver libname)))
                (let ((dir (caddr r))
                      (pth (cadddr r)))
                  (if (pair? (cdr names))
                    (list libname dir pth (cdr names) exports)
                    (list libname dir pth #f exports))))))
           order))))


