(library (guile-srfi i6)
         (export
           open-input-string
           open-output-string
           get-output-string)
         (import (only (guile) 
                       open-input-string
                       open-output-string
                       get-output-string)))

