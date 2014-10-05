;; FIXME: Tentative...
;; FIXME: Racket do not have MONOTONIC time...
(library (racket-r7b i19)
         (export 
           time-monotonic
           time-tai
           current-time
           time-second
           time-nanosecond)
         (import (rnrs) (only (racket base) current-inexact-milliseconds))

(define time-tai 'time-tai)         
(define time-monotonic 'time-monotonic)

(define (time-second ms)
  (floor (/ ms 1000.0)))

(define (time-nanosecond ms)
  (let ((sec (/ ms 1000.0)))
   (let ((residue (- sec (floor sec))))
    (* residue 1000000000.0))))

(define (current-time sym)
  (case sym
    ((time-tai time-monotonic)
     (current-inexact-milliseconds))
    (else #f)))
         
)
