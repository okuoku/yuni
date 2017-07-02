(library (yunivmrt yunicore-proc)
         (export
           make-simple-struct
           simple-struct?
           simple-struct-ref
           simple-struct-set!
           simple-struct-name
%%yunifake-dummy-syntax-yunicore-procs
           )
         (import (r7c-yunicore simple-struct))
         
;; Mark explicitly as a macro-library
(define-syntax-names/yunifake %%yunifake-dummy-syntax-yunicore-procs)
)
