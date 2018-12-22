(define (%selfboot-yuniconfig-resolver-add-loadpath! p path)
  (p (list path)))
(define (%selfboot-yuniconfig-gen-resolver impl yuniroot)
  (define cfgpath (string-append yuniroot "/config/config.scm"))
  (let* ((cfg (%selfboot-file->sexp-list cfgpath))
         (libdirs 
           (map (lambda (p) (string-append yuniroot "/" p)) 
                (cdr (assoc '*library-directories* cfg))))
         (libgrps (cdr (assoc '*library-groups* cfg)))
         (g1 (cdr (assoc 'GenRacket cfg)))
         (g2 (cdr (assoc 'GenR7RS cfg)))
         (g3 (cdr (assoc 'GenR6RSCommon cfg)))
         (implgroups (append g1 g2 g3)))
    (let ((mygroups (cdr (assoc impl implgroups))))
     (define prefixes '())
     (define (add-prefix! e)
       (cond
         ((pair? e)
          (unless (and (= (length e) 3) (eq? (cadr e) '=>)) 
            (error "Unknown libent" e))
          (let ((aliasname (car e))
                (origname (caddr e)))
            (let ((p (assoc origname prefixes)))
             (cond
               (p 
                 (let ((d (cdr p)))
                  (set-cdr! p (cons aliasname d))))
               (else
                 (set! prefixes (cons (list origname aliasname) prefixes)))))))
         (else
           (unless (symbol? e)
             (error "Unknown libent" e))
           (let ((p (assoc e prefixes)))
            (unless p
              (set! prefixes (cons (list e e) prefixes)))))))

     (for-each (lambda (group)
                 (let ((lis (cdr (assoc group libgrps))))
                  (for-each add-prefix! lis)))
               mygroups)

     (lambda (arg)
       (define (libname->path libname)
         (let ((n (reverse libname)))
          (let loop ((q (cdr n))
                     (cur (symbol->string (car n))))
            (if (null? q)
              (string-append cur ".sls")
              (loop (cdr q) 
                    (string-append (symbol->string (car q)) "/" cur))))))
       (define (try-lib libname) ;; #f / (ORIGNAME ALIASNAME DIR PTH)
         (let ((pth (libname->path libname)))
          (let loop ((q libdirs))
           (and (not (null? q))
                (let ((prefix (car q))
                      (next (cdr q)))
                  (write (list 'TRYLIB: libname prefix pth))
                  (newline)
                  (if (%selfboot-file-exists? 
                        (string-append prefix "/" pth))
                    (list arg libname prefix pth)
                    (loop next)))))))
       (let ((a (car arg))
             (d (cdr arg)))
        (cond
          ((string? a)
           (set! libdirs (cons a libdirs)))
          ((symbol? a)
           (let ((aliases (or (assoc a prefixes) (list a))))
            (let loop ((q aliases))
             (cond
               ((null? q) #f)
               (else (let ((a (car q))
                           (next (cdr q)))
                       (let ((e (try-lib (cons a d))))
                        (or e
                            (loop next)))))))))))))))

(define (%selfboot-yuniconfig-get-runtime-list impl yuniroot)
  ;; => runtime file paths relative to yuniroot
  (define cfgpath (string-append yuniroot "/config/generic-runtime.scm"))
  (let ((cfg (%selfboot-file->sexp-list cfgpath)))
   (let ((generic (assoc 'generic cfg))
         (additional (assoc impl cfg)))
     (if additional
       (append
         (cadr additional)
         (cadr generic)
         (caddr generic)
         (caddr additional))
       '()))))

