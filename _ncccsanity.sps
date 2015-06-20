(import (yuni scheme)
        (yuni ffi abi abiv0-runtime)
        (yuni compat ffi primitives))

(define mod (yuniffi-module-load "yunistub_test_primitives"))
(define xmod (yuniffi-module-load "NEVERLAND"))
(define SDL2 (yuniffi-module-load "yunistub_SDL2"))
(define trivial (yuniffi-module-load "yunistub_testing_trivial"))

(write mod)(newline)
(write xmod)(newline)
(write trivial)(newline)
(write SDL2)(newline)

(when xmod
  (error "NEVERLAND" xmod))

(when trivial
  (let* ((tbl (yuniffi-abiv0-lookup/constants trivial
                                              "testing_trivial"))
         (lis (yuniffi-abiv0-get-table tbl)))
    (for-each (lambda (e) (write e)(newline)) lis)))

(newline)

(when SDL2
  (let* ((tbl (yuniffi-abiv0-lookup/constants SDL2 "SDL2"))
         (func (yuniffi-abiv0-lookup/bridgestubs SDL2 "SDL2"))
         (lis (yuniffi-abiv0-get-table tbl))
         (lis2 (yuniffi-abiv0-get-table func)))
    (for-each (lambda (e) (write e)(newline)) lis)
    (for-each (lambda (e) (write e)(newline)) lis2)
    
    ))

(newline)

(flush-output-port (current-output-port))
