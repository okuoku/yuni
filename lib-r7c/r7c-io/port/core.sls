(library (r7c-io port core)
         (export
           port?
           input-port?
           output-port?
           textual-port?
           binary-port?)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c syntax and)
                 (r7c-yunicore yuniport))

(define port? yuniport?)
(define input-port? yuniport-input-port?)
(define output-port? yuniport-output-port?)
(define textual-port? yuniport-textual-port?)
(define binary-port? yuniport-binary-port?)
         
)
