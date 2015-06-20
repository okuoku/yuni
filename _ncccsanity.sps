(import (yuni scheme)
        (yuni compat ffi primitives))

(define mod (yuniffi-module-load "yunistub_test_primitives"))
(define xmod (yuniffi-module-load "NEVERLAND"))

(write mod)(newline)

(when xmod
  (error "NEVERLAND" xmod))



(flush-output-port (current-output-port))
