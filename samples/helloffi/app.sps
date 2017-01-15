(import (yuni scheme)
        (yuni compat bitwise primitives) ;; bv-
        (yuni compat ffi primitives) ;; yuniffi-nccc-call
        (yunistub helloffi_stub-constants))

(define (forward0stub obj)
  (let ((x (assq 'forward-0 obj)))
   (and x
        (cdr x))))

(define (call2 proc arg1 arg2)
  ;; Generate call packet
  (define in (make-bytevector (* 8 2) 0))
  (define out (make-bytevector 8 0))
  (bv-write/u64! in 0 arg1)
  (bv-write/s64! in 8 arg2)

  (yuniffi-nccc-call proc in 0 2 out 0 1)

  (bv-read/s64 out 0))

(call2 (forward0stub testfunc) 1234 -1234)

(close-port (current-output-port))
