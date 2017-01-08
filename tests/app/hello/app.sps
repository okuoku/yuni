(import (yuni scheme)
        (sub hello))

(let ((h (subhello)))
  (unless h
    (error "Unexpected."))
  (exit)) 

