(library (yunivm util compatlibs)
         (export 
           compatlibs-proc-vector
           compatlibs-name-vector)
         (import (yuni scheme)
                 (yuni compat simple-struct)) 

(define compatlibs-proc-vector
  (vector
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
    )
  )         

(define compatlibs-name-vector
  '#(
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
     ))
         
         
)
