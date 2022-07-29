(library (yuni util files)
         (export 

           ;; Gauche style pathutils
           absolute-path?
           relative-path?
           simplify-path
           path-append
           path-basename
           path-dirname
           path-extension
           path-sans-extension
           path-swap-extension
           file->list
           file->sexp-list
           file->string-list
           file->bytevector
           string-list->file

           ;; defined in file-ops
           file-regular?
           file-directory?
           directory-list
           current-directory
           create-directory
           delete-directory

           ;; nmosh utils
           expand-loadpath
           ;; yuni files
           directory-walk
           ;; directory-fold
           )
         (import (yuni scheme) 
                 (yuniconfig build)
                 (yuni compat file-ops))

;; TEMP
(define (fold-left1 proc init lis)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (let ((c (proc cur a)))
         (itr c d)))
      cur))
  (itr init lis))

(define (filter1 proc lis)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (if (proc a)
          (itr (cons a cur) d)
          (itr cur d)))
      (reverse cur)))
  (itr '() lis))

(define (put-string port str)
  (display str port))

(define (open-file-input-port nam) ;; FIXME: Use R7RS name
  (open-binary-input-file nam))

(define (get-bytevector-all port)
  (let ((p (open-output-bytevector)))
   (let loop ()
    (define bv (read-bytevector (* 1024 1024) port))
    (cond
      ((eof-object? bv)
       (get-output-bytevector p))
      (else
        (write-bytevector bv p)
        (loop))))))

(define (get-line port) ;; FIXME: Use R7RS name
  (read-line port))

;; from mosh-utils5.scm
(define (system-msdos-style-path0?)
  (let ((c (yuniconfig-platform)))
   (or (string=? "WIN32" c)
       (string=? "WIN64" c))))

(define (run-win32-np?) (system-msdos-style-path0?))
(define CHR-ENVPATHSEP (if (run-win32-np?) #\; #\:))

(define pathfilter 
  (if (run-win32-np?) 
    (lambda (str) 
      (and (string? str) 
	   (list->string (map (lambda (e) (if (char=? e #\\) #\/ e)) (string->list str)))))
    (lambda (str) str)))

(define pathfinish 
  (if (run-win32-np?)
    (lambda (str) (and (string? str) (list->string (cdr (string->list str)))))
    (lambda (str) str)))

(define do-absolute-path? 
  (if (run-win32-np?) ;FIXME: support UNC pathes
    (lambda (pl)
      (and (pair? pl)
           (let ((a (car pl)))
             (and ; is a drive letter?
               (= (string-length a) 2)
               (char=? (cadr (string->list a)) #\:)))))
    (lambda (pl) (= 0 (string-length (car pl))) )))


;;------------------------------------------------
;; utils
;;------------------------------------------------
(define (strsep str chr)
  (define (gather l) ;
    (define (itr cur rest0 rest1)
      (cond
	((not (pair? rest1)) (reverse cur))
	(else
	  (itr (cons (substring str
		       (+ 1 (car rest0)) 
		       (car rest1)) cur) 
	       (cdr rest0) 
	       (cdr rest1)))))
    (itr '() l (cdr l)))
  (define (spl l s)
    (define (itr idx cur rest)
      (cond
	((not (pair? rest)) (reverse (cons idx cur)))
	((char=? s (car rest))
	 (itr (+ idx 1) (cons idx cur) (cdr rest)))
	(else
	  (itr (+ idx 1) cur (cdr rest)))))
    (itr 0 (list -1) l))
  (if (string? str)
    (let* ((l (string->list str))
	   (m (spl l chr))
	   (r (gather m)))
      r )
    '()
    ))
;;------------------------------------------------
;; path handling
;;------------------------------------------------
(define (compose-rel-path l)
  (define (omit-dot l)
    (define (itr cur rest)
      (if (pair? rest)
	(let ((a (car rest)))
	  (if (string=? "." a)
	    (itr cur (cdr rest)) ; drop "."
	    (itr (cons a cur) (cdr rest))))
	(reverse cur)))
    (itr '() l))
  (define (omit-zerolen l)
    (define (itr cur rest)
      (if (pair? rest)
	(let ((a (car rest)))
	  (if (= 0 (string-length a))
	    (itr cur (cdr rest))
	    (itr (cons a cur) (cdr rest))))
	(reverse cur)))
    (itr '() l))
  (define (insert-slash l)
    (define (itr cur rest)
      (if (pair? rest)
	(itr (cons "/" (cons (car rest) cur)) (cdr rest))
	(reverse (cdr cur)))) ;drop last "/"
    (itr '() l))
  (apply string-append (insert-slash (omit-dot (omit-zerolen l)))))

(define (path->list pth)
  (strsep (pathfilter pth) #\/))

(define (list->path l)
  (fold-left1 path-append (car l) (cdr l)))

(define (expand-loadpath lp)
  (strsep lp CHR-ENVPATHSEP))

(define (path-append dir name) ;;FIXME: need canon.
  (string-append dir "/" name))

(define (absolute-path? pth)
  (do-absolute-path? (path->list pth)))

(define (relative-path? pth) (not (absolute-path? pth)))

(define (simplify-path pth)
  (define (sweep l)
    (define (sweep/dot l)
      (filter1 (lambda (e) (not (string=? e "."))) l))
    (define (sweep/dd cur a)
      (if (pair? a)
        (if (and (string=? (car a) "..")
                 (pair? cur)
                 (not (string=? (car cur) "..")))
          (sweep/dd (cdr cur) (cdr a))
          (sweep/dd (cons (car a) cur) (cdr a)))
        (reverse cur)))
    (sweep/dot (sweep/dd '() l)))
  (let ((l (sweep (path->list pth))))
    (if (pair? l)
      (list->path l)
      ".")))

(define (split-dir+base pth)
  (define (itr cur rest)
    (if (pair? rest)
      (if (char=? (car rest) #\/)
        (cons
          (list->string (reverse (cdr rest)))
          (list->string cur)) ;basename
        (itr (cons (car rest) cur) (cdr rest)))
      (cons "" pth)))
  (let ((p (pathfilter pth)))
    (itr '() (reverse  (string->list p)))))

(define (path-basename pth)
  (cdr (split-dir+base pth)))

(define (path-dirname pth)
  (car (split-dir+base pth)))

;; extensions
(define (path-extension pth)
  (define (itr cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (cond
          ((char=? a #\.)
           (list->string cur))
          ((char=? a #\/)
           #f)
          (else
            (itr (cons a cur) d))))
      #f))
  (itr '() (reverse (string->list pth))))

(define (path-sans-extension pth)
  (define ext (path-extension pth))
  (if (not ext)
    pth
    (substring pth 0 (- (string-length pth) 
                        1  ;; for "."
                        (string-length ext)))))

(define (path-swap-extension pth newext)
  (define basename (path-sans-extension pth))
  (if (not newext)
    basename
    (string-append basename "." newext)))

(define (file->list proc pth)
  (call-with-input-file
    pth
    (lambda (p)
      (define (itr cur)
        (let ((r (proc p)))
          (if (eof-object? r)
            (reverse cur)
            (itr (cons r cur)))))
      (itr '()))))

(define (file->sexp-list pth)
  (file->list read pth))

(define (file->string-list pth)
  (file->list get-line pth))

(define (file->bytevector pth)
  (define p (open-file-input-port pth))
  (define bv (get-bytevector-all p))
  (close-port p)
  (if (eof-object? bv) (make-bytevector 0) bv))

;; To file op .. Always overwrite
(define (string-list->file pth l)
  (when (file-exists? pth)
    (delete-file pth))
  (call-with-output-file
    pth
    (lambda (p)
      (for-each (lambda (e) 
                  (put-string p e)
                  (newline p))
                l))))


;; tree walk
(define (directory-walk pth proc)
  (define (do-walk base cur)
    (define my-path (path-append base cur))
    (cond
      ((and (file-directory? my-path)
            (not (string=? cur ".."))
            (not (string=? cur ".")))
       (directory-walk my-path proc))
      ((and (file-regular? my-path)
            (not (file-directory? my-path)))
       (proc my-path))))
  (for-each (lambda (e) (do-walk pth e)) (directory-list pth)))


)


