;; Port output utility. FIXME: Move this as public library

(library (yuni ffi ctemplate util)
         (export
           put-obj)
         (import (yuni scheme))

(define (recons obj)
  ;; FIXME: To prevent Racket's { } list output
  (define (listproc p obj)
    (if (pair? obj)
      (let ((a (car obj))
            (d (cdr obj)))
        (put-obj p a)
        (when (pair? d)
          (display " " p))
        (listproc p d))
      (get-output-string p)))

  (cond
    ((list? obj)
     (string-append "("
                    (listproc (open-output-string) obj)
                    ")"))
    (else obj)))

(define (put-obj port . objs)
  (for-each (lambda (e) (display (recons e) port)) objs))

)
