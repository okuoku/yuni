(import (rnrs)
        (ironscheme clr)
        (ironscheme))

(define cmdline
  (if (> 2 (length (command-line)))
    (assertion-violation 'init
                         "missing filename")
    (cdr (command-line))))

(define libpaths '())
(define filename '())
(define args '())

(define compile-mode #f)

(define (init-args!)
  (when (pair? cmdline)
    (let ((a (car cmdline))
          (d (cdr cmdline)))

      ;; set args
      (set! filename a)
      (set! args d)

      ;; Filter-out -I options
      (when (string=? a "-I")
        (let ((dir (car d))
              (next (cdr d)))
          (set! cmdline next)
          (set! libpaths (cons dir libpaths))
          (init-args!)))
      (when (string=? a "-COMPILE")
        (set! compile-mode #t)
        (set! cmdline d)
        (init-args!)))))

(define (run-with-guard thunk)
  (define err (current-output-port))
  (clr-guard (e (e (display "Unhandled CLR exception:\n" err)
                   (display e err)
                   (newline err)
                   (exit 2)))
             (with-exception-handler
               (lambda (e)
                 (display "Unhandled exception:\n" err)
                 (display e err)
                 (newline err)
                 (exit 1))
               thunk)))


(init-args!)

(run-with-guard ;; FIXME: Ineffective for compile.
  (lambda ()
    (parameterize ((library-path (append libpaths
                                         (library-path)))
                   (command-line args))
                  (cond
                    (compile-mode
                      (compile filename #f #f))
                    (else
                      (let ((thunk (compile->closure filename)))
                       (thunk)))))))

