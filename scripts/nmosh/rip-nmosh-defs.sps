(import (rnrs) (match) (yuni util files))
#|
(import (yuni scheme)
        (yuni util files)
        (yuni base match))
|#

(define coredefs
  '(%verbose
     %disable-acc
     %nmosh-portable-mode
     %nmosh-prefixless-mode
     %loadpath
     %getpid))

(define NMOSH-FILES* 
  '("../../../mosh/boot/runtimes/srfi-mosh/compat-mosh-run.scm"
    "../../../mosh/boot/runtimes/srfi-mosh/runtime.scm"
    "../../../mosh/boot/runtimes/srfi-mosh/mosh-utils5.scm"))

(define nmosh-files (apply append (map (lambda (e)
                                         (file->sexp-list e))
                                       NMOSH-FILES*)))

(define names coredefs)

(define (proc e)
  (match e
         (('define (name . bogus1) . bogus2)
          (set! names (cons name names)))
         (('define name . bogus)
          (set! names (cons name names)))
         (else 'do-nothing)))

(for-each proc nmosh-files)

(for-each (lambda (e) (display e)(newline)) names)
(newline)
