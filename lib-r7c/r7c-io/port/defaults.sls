(library (r7c-io port defaults)
         (export
           current-input-port
           current-output-port
           current-error-port
           with-input-from-file
           with-output-to-file
           )
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c syntax and)
                 (r7c-yunicore yuniport))

(define (current-input-port) #f)
(define (current-output-port) #f)
(define (current-error-port) #f)
         
)
