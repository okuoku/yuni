(library (yuni ffi database prologue)
         (export make-prologue
                 prologue-source
                 prologue-symbols
                 prologue-add-symbol!)
         (import (yuni scheme)
                 (yuni core))

;; prologue. There is no information for scheme runtime.

(define* prologue (source symbols*))

(define (make-prologue src)
  (make prologue
        (source src)
        (symbols* '())))

(define* (prologue-source (prologue))
  (~ prologue 'source))

(define* (prologue-symbols (prologue))
  (~ prologue 'symbols*))

(define* (prologue-add-symbol! (prologue) sym)
  (~ prologue 'symbols* := (cons sym (prologue-symbols prologue))))

)
