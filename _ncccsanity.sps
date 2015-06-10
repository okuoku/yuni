(import (yuni scheme)
        (yuni compat ffi primitives))

(define mod (yuniffi-module-load "yunistub_test_primitives"))

(write mod)(newline)
