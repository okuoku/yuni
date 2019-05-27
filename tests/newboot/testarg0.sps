;; Run me with "SPLITHERE" arg
(import (yuni scheme)
        (yunitest mini))

(define cmd (command-line))

(check-equal #t (and (member "SPLITHERE" cmd) #t))

(check-finish)

