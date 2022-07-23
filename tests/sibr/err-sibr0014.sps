(import (yuni scheme))

(call-with-values
  (lambda () (values))
  (lambda () 'bogus))
