(import (rnrs)
        (larceny compiler))

(define file (caddr (command-line)))

(compile-file file)
