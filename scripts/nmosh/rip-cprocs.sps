(import (rnrs) (match) (yuni util files))
#|
(import (yuni scheme)
        (yuni util files)
        (yuni base match))
|#

(define CPROCS-FILE "../../../mosh/boot/free-vars.scm")
(define cprocs (cdar (file->sexp-list CPROCS-FILE)))

(define names '())

(define (proc e)
  (match e
         ((name . bogus)
          (set! names (cons name names)))
         (name
          (set! names (cons name names)))))

(for-each proc cprocs)

(for-each (lambda (e) (display e)(newline)) names)
(newline)
