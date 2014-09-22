(library (yunife loader)
         (export
           make-library-provider)
         (import (yuni scheme)
                 (yunife library-provider file)
                 (yunife library-provider alias))

(define (make-library-provider) ;; => ^(name ^(libname file))
  (define file (make-file-library-provider 
                 '("lib-yunife-stub" "lib")
                 ".sls"))
  (define provider (make-alias-library-provider '() file))
  (lambda (name k)
    (provider name k)))         

)
