(library (r7c-io port control)
         (export
           flush-output-port
           input-port-open?
           output-port-open?
           close-port
           close-input-port
           close-output-port
           )
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c syntax and)
                 (r7c-yunicore yuniport))

         
(define flush-output-port yuniport-flush)
(define input-port-open? yuniport-input-port-open?)
(define output-port-open? yuniport-output-port-open?)
(define close-port yuniport-close)
(define close-input-port yuniport-close-input-port)
(define close-output-port yuniport-close-output-port)
)
