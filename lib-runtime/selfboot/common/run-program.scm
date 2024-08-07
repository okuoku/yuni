(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define %%selfboot-current-modpath '())
(define (yuni/command-line) %%selfboot-current-command-line)

;; Scan arguments
(let loop ((q %%selfboot-current-command-line))
 (if (pair? q)
   (let ((a (car q))
         (d (cdr q)))
     (cond
       ((string=? "-LIBPATH" a)
        (let ((dir (car d)))
         (set! %%selfboot-current-libpath
           (cons dir
                 %%selfboot-current-libpath))
         (loop (cdr d))))
       ((string=? "-MODPATH" a)
        (let ((dir (car d)))
         (set! %%selfboot-current-modpath
           (cons dir
                 %%selfboot-current-modpath))
         (loop (cdr d))))
       (else
         (set! %%selfboot-current-command-line q))))
   #t))

;; Collect deps and load
(let ((r (yuni/command-line)))
 (let* ((prog (car r))
        (codetab (%%selfboot-gen-filelist 
                   %%selfboot-current-libpath
                   (list prog))))
   (for-each (lambda (e)
               (let ((dir (cadr e))
                     (pth (caddr e))
                     (truename (car e))
                     (aliasnames (cadddr e))
                     (libexports (car (cddddr e))))
                 (cond
                   ((pair? libexports)
                    (let ((raw-imports (car libexports))
                          (exports (cdr libexports))
                          (filepth (string-append dir "/" pth)))
                      ;(write (list 'LOAD: filepth)) (newline)
                      (%%selfboot-loadlib filepth truename raw-imports exports)
                      (when aliasnames
                        ;(write (list 'ALIAS: truename '=> aliasnames exports))
                        ;(newline)
                        (%%selfboot-load-aliaslib 
                          truename aliasnames exports))))
                   (else ;; Polyfill
                     (let ((filepth (string-append dir "/" pth)))
                      ;; Special command to load polyfill
                      (%%selfboot-loadlib filepth libexports #t #t))))))
             codetab)
   (%%selfboot-load-program prog)))

;; Exit successfully if the program does not have exit call
(exit 0)
