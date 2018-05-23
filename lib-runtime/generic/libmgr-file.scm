;;
;; yuni library manager: File-based library provider
;; 

; Globals

;; Directory list for library
(define *yuni/library-import-dirs* '())


; Procedures

;; Generate library path for a library name
(define (yuni/make-library-path base nam)
  (if (pair? nam)
    (yuni/make-library-path (string-append (string-append base "/")
                                           (symbol->string (car nam)))
                            (cdr nam))
    (string-append base ".sls")))

;; Try to lookup library
(define (yuni/library-name->path name) ;; => path | #f
  (define (itr rest)
    (and (pair? rest)
         (or (let ((name (yuni/make-library-path (car rest) name)))
              (PCK 'TRYING: name)
              (and (file-exists? name)
                   name))
             (itr (cdr rest)))))
  (PCK 'LOOKUP: name)
  (itr *yuni/library-import-dirs*))

(define (yuni/add-library-import-dir! dir)
  (set! *yuni/library-import-dirs*
    (cons dir
          *yuni/library-import-dirs*)))


