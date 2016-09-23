(library (racket-r7b i6bv)
         (export
           ;;open-input-bytevector
           open-output-bytevector
           get-output-bytevector)
         (import (rename
                   (only (racket base) open-output-bytes get-output-bytes)
                   (open-output-bytes open-output-bytevector)
                   (get-output-bytes  get-output-bytevector))))

