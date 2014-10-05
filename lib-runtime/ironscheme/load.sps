(import (rnrs)
        (ironscheme))
(define cmdline
  (if (> 2 (length (command-line)))
    (assertion-violation 'init
                         "missing filename")
    (command-line)))
(define filename (cadr cmdline))
(define args (cdr cmdline))

(parameterize ((library-path (append 
                               '("lib-r6rs" "lib" "lib-stub"
                                 "lib-compat"
                                 "lib-stub/ironscheme") 
                               (library-path)))
               (command-line args))
              (load filename))
