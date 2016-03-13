(import (rnrs) (match) (yuni util files))
#|
(import (yuni scheme)
        (yuni util files)
        (yuni base match))
|#

(define BASELIB-FILE "../../../mosh/boot/baselib.scm")
(define baselib (file->sexp-list BASELIB-FILE))

(define names '())

(define (proc e)
  (match e
         (('define (name . bogus1) . bogus2)
          (set! names (cons name names)))
         (('define name . bogus)
          (set! names (cons name names)))
         (else 'do-nothing)))

(for-each proc baselib)

(for-each (lambda (e) (display e)(newline)) names)
(newline)
