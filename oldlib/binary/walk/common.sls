(library (yuni binary walk common)
         (export entry type network link-entry
                 sym-cstruct
                 sym-csize)
         (import (rnrs)
                 (shorten)
                 (srfi :48)
                 (yuni core))

;;

(define* network
  (name
    type ;; list(null terminated)
    ;; FIXME: Currently, only uniform lists are supported
    head-first ;; entry-name
    next ;; entry-name | list
    prev ;; entry-name | list
    ))

;; Link entry has separate class. This will be expanded later.
(define* link-entry
  (name
    parent
    link-head? ;; boolean
    link-class ;; sym
    ))

(define* entry
  (name
    ;; Filled via specfile
    type ;; integer | string 
    constant? ;;
    parent ;; object-name | #t(global object) | list (struct)
    count ;; => entry
    ;; Filled via layout
    value ;; if constant (NB: not a offset)
    offset ;; Offset in parent struct
    size))

(define* type
  (name
    type ;; c-struct | struct | flags
    entries* ;; entry-name*
    ))

(define (sym-cstruct struct-name l)
  (string->symbol
    (format "STR_~a_~a"
            struct-name
            (fold-left
              (^[cur e]
                (format "~a_~a" cur e))
              (symbol->string (car l))
              (cdr l)))))

(define (sym-csize name)
  (string->symbol (format "SIZEXX_~a" name)))

         
)
