(library (guile-r7b i19)
         (export 
           time-monotonic
           time-tai
           current-time
           time-second
           time-nanosecond)   
         (import (srfi :19)))
