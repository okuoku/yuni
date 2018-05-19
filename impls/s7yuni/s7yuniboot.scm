(display "This is yuniboot.\n" (current-error-port))
(display "Cmdline: " (current-error-port))
(write *command-line* (current-error-port))
(newline (current-error-port))

;; Global variables

(define yuni/rawmode #f)
(define yuni/root #f)
(define yuni/script #f)
(define yuni/args '())

;; Command-line procedure

(let loop ((cur (cdr *command-line*)))
 (when (pair? cur)
   (let ((arg (car cur)))
    (cond ((string=? arg "-YUNIROOT")
           (when (pair? (cdr cur))
             (set! yuni/root (cadr cur))
             (loop (cddr cur))))
          ((string=? arg "-RAWMODE")
           (set! yuni/rawmode #t)
           (loop (cdr cur)))
          (else
            (set! yuni/script arg)
            (set! yuni/args (cdr cur)))))))

(define (command-line) yuni/args)

(cond
  (yuni/rawmode 
    (load yuni/script))
  (else
    (load yuni/script)))

(exit 0)
