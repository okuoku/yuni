(import (rnrs)
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


(init-args!)

(parameterize ((library-path (append libpaths
                                     (library-path)))
               (command-line args))
              (cond
                (compile-mode
                  (compile filename #f #f))
                (else
                  (load filename))))
