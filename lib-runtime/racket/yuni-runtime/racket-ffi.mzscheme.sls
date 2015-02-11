#!r6rs
(library (yuni-runtime racket-ffi)
         (export nccc-func)
         (import (rnrs)
                 (ffi unsafe)) 
(define nccc-func
  (_fun _gcpointer _int _gcpointer _int -> _void))

)
