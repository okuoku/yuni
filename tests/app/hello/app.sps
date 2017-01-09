(import (yuni scheme)
        (sub hello))

(let ((h (subhello)))
  (unless h
    (error "Unexpected."))
  ;; For Vicare
  (close-port (current-output-port))
  (exit)) 

