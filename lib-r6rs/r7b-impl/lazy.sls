(library (r7b-impl lazy)
         (export delay force
                 (rename (eager make-promise)
                         (lazy delay-force))
                 promise?)
         (import (r7b-util s45))
)
