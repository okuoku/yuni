(define %%selfboot-current-command-line %%selfboot-program-args)
(define %%selfboot-current-libpath (list %%selfboot-yuniroot))
(define (command-line) %%selfboot-current-command-line)

;; Scan arguments

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
                     (exports (car (cddddr e))))
                 (let ((filepth (string-append dir "/" pth)))
                  (write (list 'LOAD: filepth)) (newline)
                  (load filepth)
                  (when aliasnames
                    ;(write (list 'ALIAS: truename '=> aliasnames exports))
                    ;(newline)
                    (%%selfboot-load-aliaslib truename aliasnames exports)))))
             codetab)
   (load prog)))

;; Exit successfully if the program does not have exit call
(exit 0)