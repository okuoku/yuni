(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define (command-line) %%selfboot-current-command-line)

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
       (else
         (set! %%selfboot-current-command-line q))))
   #t))

;; Collect deps and load
(let ((r (command-line)))
 (let* ((prog (car r))
        (codetab (%%selfboot-gen-filelist 
                   %%selfboot-current-libpath
                   (list prog))))
   (for-each (lambda (e)
               (let ((dir (cadr e))
                     (pth (caddr e))
                     (truename (car e))
                     (aliasnames (cadddr e))
                     (raw-imports (car (car (cddddr e))))
                     (exports (cdr (car (cddddr e)))))
                 (let ((filepth (string-append dir "/" pth)))
                  (write (list 'LOAD: filepth)) (newline)
                  (%%selfboot-loadlib filepth truename raw-imports exports)
                  (when aliasnames
                    ;(write (list 'ALIAS: truename '=> aliasnames exports))
                    ;(newline)
                    (%%selfboot-load-aliaslib truename aliasnames exports)))))
             codetab)
   (%%selfboot-load-program prog)))

;; Exit successfully if the program does not have exit call
(exit 0)
