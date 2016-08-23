(library (gambit-yuni compat ffi primitives-phase1)
         (export
           yuniffi-nccc-call
           ;; for phase2
           dlopen
           dlsym)
         (import (yuni scheme)
                 (yuni ffi runtime bootstraploader)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (gambit-yuni compat ffi primitives-phase0))

(define-primitive-names/yunifake
  %%yuniffi-nccc-call
  %%yuniffi-nccc-bootstrap)

(define (yuniffi-nccc-call . args)
  (ensure-yuniffi-loaded!)
  (apply %%yuniffi-nccc-call args))

(define (yuniffi-nccc-bootstrap)
  (ensure-yuniffi-loaded!)
  (%%yuniffi-nccc-bootstrap))

(define dlfuncs
  (call-with-values
    (lambda ()
      (make-bootstraploader %%yuniffi-nccc-call
                            %%yuniffi-nccc-bootstrap
                            ptr?
                            bv-read/w64ptr
                            bv-write/w64ptr!
                            ptr-write/asciiz!))
    (lambda (xdlopen xdlsym) (cons xdlopen xdlsym))))

(define (dlopen pth) ((car dlfuncs) pth))
(define (dlsym handle str) ((cdr dlfuncs) handle str))

)
