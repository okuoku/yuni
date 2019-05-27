;; Run me with "SPLITHERE" "a" "1" "c" arg
(import (yuni scheme)
        (yunitest mini))

(define cmd (command-line))

(check-equal '("SPLITHERE" "a" "1" "c") (member "SPLITHERE" cmd))

(check-finish)

