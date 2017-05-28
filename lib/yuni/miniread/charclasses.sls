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
    (32 SPACE)
    (9 TAB)
    (13 CR)
    (10 LF)

    ;; Delimiters
    (40 PAREN_L)
    (41 PAREN_R)
    (91 SQ_L)
    (93 SQ_R)
    (59 SEMICOLON)
    (35 SHARP)
    (34 DQUOTE)

    ;; Other special characters
    (92 BACKSLASH)
    (39 FQUOTE)
    (64 AT)
    (44 COMMA)
    (124 PIPE)
    (46 DOT)
    (96 BQUOTE)

    ;; Ordinal alphabets
    (116 SMALL-T)
    (102 SMALL-F)
    (84 LARGE-T)
    (70 LARGE-F)
    ))
)
