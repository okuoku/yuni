(library (yuni miniread charclasses)
         (export
           ssplit-byte-delimiter?
           ssplit-byte-whitespace?
           ssplit-byte-class
           )
         (import (yuni scheme))

;; Return a character class symbol for a byte

(define-syntax %bcdef
  (syntax-rules ()
    ((_ (char sym) ...)
     (let ((sym char)
           ...)
       (lambda (b)
         (cond 
           ((= sym b) 'sym)
           ...
           (else #f)))))))

(define (ssplit-byte-whitespace? byte)
  (case (ssplit-byte-class byte)
    ((SPACE TAB CR LF) #t)
    (else #f)))

(define (ssplit-byte-delimiter? byte)
  (or (ssplit-byte-whitespace? byte)
      (case (ssplit-byte-class byte)
        ((PAREN_L PAREN_R SQ_L SQ_R SEMICOLON SHARP DQUOTE) #t)
        (else #f))))

(define ssplit-byte-class
  (%bcdef
    ;; Whitespaces
    (#x20 SPACE)
    (9 TAB)
    (13 CR)
    (10 LF)

    ;; Delimiters
    (#x28 PAREN_L)
    (#x29 PAREN_R)
    (#x5B SQ_L)
    (#x5D SQ_R)
    (#x3b SEMICOLON)
    (#x23 SHARP)
    (#x22 DQUOTE)

    ;; Other special characters
    (#x5c BACKSLASH)
    (#x27 QUOTE)
    (#x40 AT)
    (#x2c COMMA)
    (#x7c PIPE)
    ;(#\. DOT) ;; FIMXE: Not needed??
    (#x60 BQUOTE)

    ;; Ordinal alphabets
    (#x74 SMALL-T)
    (#x66 SMALL-F)
    (#x54 LARGE-T)
    (#x46 LARGE-F)
    ))
)
