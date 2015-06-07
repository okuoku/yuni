(library (chibi-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-module-load
           yuniffi-module-lookup
           yuniffi-module-path)
         (import (yuni scheme)
                 ;; FIXME: Undocumented?
                 (only (chibi) current-module-path)
                 ;; FIXME: Rename this?
                 (yuniffi-runtime))

;;

(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (yuniffi_nccc_call func in in-offset in-size out out-offset out-size))

(define (yuniffi-module-load path) #t)

(define (yuniffi-module-lookup handle str)
  ;; FIXME: testing
  (yuniffi_nccc_bootstrap))

(define (yuniffi-module-path) (current-module-path))
         
)
