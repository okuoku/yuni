(library (chez-srfi i19)
         (export 
           time-monotonic
           time-tai
           current-time
           time-second
           time-nanosecond)
         (import (chezscheme))

(define time-monotonic 'time-monotonic)
(define time-tai 'time-utc))
