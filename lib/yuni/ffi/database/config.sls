(library (yuni ffi database config)
         (export make-config
                 config-stub-c
                 config-stub-cxx)
         (import (yuni scheme)
                 (yuni core))

;; config object (for stubir0)
(define* config (stub-c stub-cxx))

(define (make-config stub-c stub-cxx)
  (make config
        (stub-c stub-c)
        (stub-cxx stub-cxx)))

(define* (config-stub-c (config))
  (~ config 'stub-c))

(define* (config-stub-cxx (config))
  (~ config 'stub-cxx))
)
