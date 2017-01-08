(import (yuni scheme)
        (hello)
        (sub hello))

(let ((h (hello)))
 (let ((i (subhello)))
  (unless (and h i)
    (error "Unexpected."))
  (exit))) 

