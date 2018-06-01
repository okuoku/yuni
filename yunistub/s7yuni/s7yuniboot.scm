(display "This is yuniboot.\n" (current-error-port))
(display "Cmdline: " (current-error-port))
(write *command-line* (current-error-port))
(newline (current-error-port))

;; Global variables

(define yuni/rawmode #f)
(define yuni/root #f)
(define yuni/script #f)
(define yuni/args '())

;; Command-line procedures
(define (yuni/update-command-line! lis) (set! yuni/args lis))
(define (yuni/command-line) yuni/args)
(define (command-line) yuni/args)

;; Process command line

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
            (cond
              (yuni/root
                (set! yuni/args cur)
                (set! yuni/script #f))
              (else
                (set! yuni/script arg)
                (set! yuni/args (cdr cur)))))))))

(cond
  ((or yuni/rawmode (not yuni/root)) 
    (load yuni/script))
  (else
    (load yuni/root)))

(exit 0)
