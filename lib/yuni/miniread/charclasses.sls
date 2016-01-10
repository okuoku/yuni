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
     (let ((sym (if (char? char) (char->integer char) char))
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
    (#\space SPACE)
    (9 TAB)
    (13 CR)
    (10 LF)

    ;; Delimiters
    (#\( PAREN_L)
    (#\) PAREN_R)
    (#\[ SQ_L)
    (#\] SQ_R)
    (#\; SEMICOLON)
    (#\# SHARP)
    (#\" DQUOTE)

    ;; Other special characters
    (#\\ BACKSLASH)
    (#\' QUOTE)
    (#\@ AT)
    (#\, COMMA)
    (#\| PIPE)
    ;(#\. DOT) ;; FIMXE: Not needed??
    (#\` BQUOTE)

    ;; Ordinal alphabets
    (#\t SMALL-T)
    (#\f SMALL-F)
    (#\t LARGE-T)
    (#\f LARGE-F)

    ))
)
