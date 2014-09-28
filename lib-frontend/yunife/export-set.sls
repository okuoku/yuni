(library (yunife export-set)
         (export
           make-builtin-eset
           make-eset)
         (import (yuni scheme)
                 (yunife build-id))

(define (make-builtin-eset) (cons #f '()))
(define (make-eset name) (cons (string->symbol
                                 (string-append build-id
                                                "~"
                                                (symbol->string name))) '()))

)
