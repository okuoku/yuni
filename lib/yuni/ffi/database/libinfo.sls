(library (yuni ffi database libinfo)
         (export make-libinfo
                 libinfo-c-name
                 libinfo-scheme-name)
         (import (yuni scheme)
                 (yuni core))

;; libinfo object         
(define* libinfo (c-name scheme-name))         

(define (make-libinfo c-name scheme-name)
  (make libinfo
        (c-name c-name)
        (scheme-name scheme-name)))

(define* (libinfo-c-name (libinfo))
  (~ libinfo 'c-name))

(define* (libinfo-scheme-name (libinfo))
  (~ libinfo 'scheme-name))
)
