(import (rnrs)
        (larceny compiler))

(define file (cadr (command-line)))

(compile-file file)
