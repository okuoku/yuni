(library (gambit-yuni compat ffi primitives-phase2)
         (export
           yuniffi-module-load
           yuniffi-module-lookup)
         (import (yuni scheme)
                 (yuni ffi runtime bootstraploader)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives)
                 (gambit-yuni compat ffi primitives-phase0)
                 (gambit-yuni compat ffi primitives-phase1))

(define-primitive-names/yunifake
  %%yuniffi-gambit-modpath)

(define yuniffi-module-load (make-simpleloader 
                              (lambda () 
                                (list %%yuniffi-gambit-modpath))
                              (lambda (pth)
                                (dlopen pth))))

(define (yuniffi-module-lookup handle str) (dlsym handle str))

)
