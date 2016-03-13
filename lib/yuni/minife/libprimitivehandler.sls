(library (yuni minife libprimitivehandler)
         (export make-libprimitivehandler)
         (import (yuni scheme)
                 (yuni minife interfacelib))

(define (make-libprimitivehandler)
  (lambda (op lib)
    (unless (eq? op 'lookup)
      (error "Unknown op" op))
    (and (equal? '(r7c-expander-interface) lib)
         (cadr (interfacelib)))))
         
)
