(library (larceny-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-module-load
           yuniffi-module-lookup)
         (import (yuni scheme)
                 ;; FIXME: Move them into runtime
                 (primitives
                   sizeof:pointer
                   ffi/handle->address
                   foreign-file
                   foreign-procedure))

;; NB: It seems Larceny's FFI do not support any namespacing..

(define (yuniffi-nccc-call func 
                           in in-offset in-size 
                           out out-offset out-size)
  ;; NB: We assume 'in' and 'out' are automagically GC-protected.
  ;; NB: It seems there is no public function for ffi/handle->address
  ;; Ref:
  ;;   larceny/lib/Ffi/memory.sch
  ;;   larceny/src/Rts/Sys/syscall.c
  ;;   larceny/include/Sys/macros.h
  (define in-addr (+ (* sizeof:pointer (+ 1 in-offset)) ;; 1 = Skip header
                     (ffi/handle->address in)))
  (define out-addr (+ (* sizeof:pointer (+ 1 out-offset)) ;; 1 = Skip header
                      (ffi/handle->address out)))
  (func in-addr in-size out-addr out-size))

(define (yuniffi-module-load path)
  (foreign-file path)
  ;; No FFI module handle on Larceny
  #t)

(define (yuniffi-module-lookup handle str) ;; => procedure
  ;; FIXME: Wow, no pointer type! Seriously?
  (foreign-procedure str '(int int int int) 'void))
         
)
