(library (chez-r7b-util string-buffer)
         (export get-output-string
                 open-output-string)
         (import (only (chezscheme)
                       get-output-string
                       open-output-string)))
