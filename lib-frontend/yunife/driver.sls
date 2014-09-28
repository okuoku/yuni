;; FIXME: TENTATIVE I/F
(library (yunife driver)
         (export
           load-file!
           make-driver)
         (import (yuni scheme)
                 (yuni base match)
                 (yunife core)
                 (yunife library-provider file)
                 (yunife library-provider alias))

(define (make-driver)
  (define library-stack '())
  (define file (make-file-library-provider 
                 '("lib-yunife" "lib")
                 ".sls"))
  (define provider (make-alias-library-provider '() file))
  (lambda x
    (match x
           (('load: filename)
            'FIXME-do-nothing
            
            ))))

(define (load-file! loader filename)
  (loader 'load: filename))

)
