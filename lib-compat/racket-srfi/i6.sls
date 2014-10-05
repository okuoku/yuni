(library (racket-srfi i6)
         (export
           open-output-string
           open-input-string
           open-output-string
           get-output-string)
         (import (rnrs)
                 (rename (only (racket base) 
                               open-output-string
                               open-input-string
                               get-output-string)
                         (open-output-string base:open-output-string)
                         (open-input-string base:open-input-string)))

;; Racket requires every R6RS port transcoded..
(define (open-output-string)
  (define p (base:open-output-string))
  (transcoded-port p (native-transcoder)))         
(define (open-input-string str)
  (define p (base:open-input-string str))
  (transcoded-port p (native-transcoder)))
         
)
