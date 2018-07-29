;;
;; yuni library manager: File-based library provider
;; 

; Globals

;; Directory list for library
(define *yuni/library-import-dirs* '())
;; Possible overrider for a library
(define *yuni/library-override-prefixes* '())
(define *yuni/library-override-paths* '())

; Procedures

;; Generate library path for a library name
(define (yuni/make-library-path base nam)
  (if (pair? nam)
    (yuni/make-library-path (string-append (string-append base "/")
                                           (symbol->string (car nam)))
                            (cdr nam))
    (string-append base ".sls")))

;; Try to lookup library
(define (yuni/library-name->path/core exists? prefix* name) ;; => path | #f
  (define (itr rest)
    (and (pair? rest)
         (or (let ((name (yuni/make-library-path (car rest) name)))
              (PCK 'TRYING: name)
              (and (exists? name)
                   name))
             (itr (cdr rest)))))
  (PCK 'LOOKUP: name)
  (itr prefix*))

(define (yuni/library-name->path0 name)
  (yuni/library-name->path/core file-exists? *yuni/library-import-dirs* name)) 

(define (yuni/library-name->path/override name)
  (define (override? fn)
    (define (itr cur)
      (and (pair? cur)
           (or (string=? (car cur) fn)
               (itr (cdr cur)))))
    (itr *yuni/library-override-paths*))
  (let ((a (car name))
        (d (cdr name)))
    (let ((m (assoc a *yuni/libalias*)))
     (and m
          (let ((aliasname (cons (cdr m) d)))
           (yuni/library-name->path/core 
             override? 
             *yuni/library-override-prefixes*
             aliasname))))))

(define (yuni/library-name->path name)
  (if (null? *yuni/library-override-paths*)
    (yuni/library-name->path0 name)
    (if (null? *yuni/library-override-prefixes*)
      (error "????")
      (or (yuni/library-name->path/override name)
          (yuni/library-name->path0 name)))))

(define (yuni/add-library-import-dir! dir)
  (set! *yuni/library-import-dirs*
    (cons dir
          *yuni/library-import-dirs*)))

(define (yuni/add-library-override-path! pth)
  (set! *yuni/library-override-paths*
    (cons pth
          *yuni/library-override-paths*)))

(define (yuni/add-library-override-prefix! prefix)
  (set! *yuni/library-override-prefixes*
    (cons prefix
          *yuni/library-override-prefixes*)))
