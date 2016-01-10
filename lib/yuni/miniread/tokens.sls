(library (yuni miniread tokens)
         (export
           ;; Normal context
           ssplit-parse-byte0
           ssplit-parse-byte1
           ssplit-parse-byte2

           ;; String context
           ssplit-instring-parse-byte0
           ssplit-instring-parse-byte1
           
           ;; Line comment context
           ssplit-incomment-parse-byte0
           ssplit-incomment-parse-byte1

           ;; Block comment context
           ssplit-inblockcomment-parse-byte0
           ssplit-inblockcomment-parse-byte1)
         (import (yuni scheme)
                 (yuni miniread charclasses))

(define-syntax %expand-clause
  (syntax-rules ()
    ((_ single)
     (values 'single #f))
    ((_ has-next #t)
     (values 'has-next #t) )))

(define-syntax %dispatch0
  (syntax-rules (=>)
    ((_ byte others (sym => clause ...) ...)
     (let ((cls (ssplit-byte-class byte)))
      (case cls
        ((sym)
         (%expand-clause clause ...))
        ...
        (else
          (others cls)))))))

(define-syntax %dispatch
  (syntax-rules (=>)
    ((_ prev-sym byte others (from to => clause ...) ...)
     (let ((cls (ssplit-byte-class byte)))
      (cond
        ((and (eq? prev-sym 'from) (eq? cls 'to))
         (%expand-clause clause ...))
        ...
        (else (others cls)))))))
         
;; Normal context
(define (ssplit-parse-byte0 byte)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch0 byte others
    ;; Paren
    (PAREN_L => LIST_BEGIN_PAREN)
    (PAREN_R => LIST_END_PAREN)
    (SQ_L => LIST_BEGIN_SQ)
    (SQ_R => LIST_END_SQ)
    ;; Quotes
    (QUOTE => NEXT_QUOTE)
    (BQUOTE => NEXT_QUASIQUOTE)
    (COMMA => NEXT_UNQUOTE #t) ;; Unquote splicing
    ;; Spaces
    (SPACE => SPACE)
    (CR => CR #t) ;; CR LF
    (LF => LF)
    ;; Specials
    (SHARP => SHARP #t) ;; #t #f ...
    (SEMICOLON => SEMICOLON)
    (DQUOTE => DQUOTE)
    ))

(define (ssplit-parse-byte1 byte prev-sym)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch prev-sym byte others
    (SHARP QUOTE => NEXT_SYNTAX_QUOTE)
    (SHARP BQUOTE => NEXT_SYNTAX_QUASIQUOTE)
    (SHARP COMMA => NEXT_SYNTAX_UNQUOTE #t) ;; splicing
    (NEXT_UNQUOTE AT => NEXT_UNQUOTE_SPLICING)
    (SHARP BACKSLASH => NEXT_CHAR_LITERAL)
    (SHARP PIPE => BLOCK_COMMENT_BEGIN)
    (SHARP SEMICOLON => NEXT_DATUM_COMMENT)
    (CR LF => CRLF)
    (SHARP SMALL-T => TRUE)
    (SHARP SMALL-F => FALSE)
    (SHARP LARGE-T => TRUE)
    (SHARP LARGE-F => FALSE)))

(define (ssplit-parse-byte2 byte prev-sym)
  (define (others cls) (values 'OTHERS #f))

  (%dispatch prev-sym byte others
    (NEXT_SYNTAX_UNQUOTE AT => NEXT_SYNTAX_UNQUOTE_SPLICING)))

;; String context
(define (ssplit-instring-parse-byte0 byte)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch0 byte others
    (BACKSLASH => BACKSLASH #t)
    (DQUOTE => DQUOTE)
    (CR => CR #t) ;; CR LF
    (LF => LF)))

(define (ssplit-instring-parse-byte1 byte prev-sym)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch prev-sym byte others
    (CR LF => CRLF)
    (BACKSLASH DQUOTE => ESCAPE_DQUOTE)))

;; Line comment context
(define (ssplit-incomment-parse-byte0 byte)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch0 byte others
    (CR => CR #t) ;; CR LF
    (LF => LF)
    (SEMICOLON => SEMICOLON)))

(define (ssplit-incomment-parse-byte1 byte prev-sym)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch prev-sym byte others
    (CR LF => CRLF)))

;; Block comment context
(define (ssplit-inblockcomment-parse-byte0 byte)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch0 byte others
    (PIPE => PIPE #t) ;; Block comment end
    (SHARP => SHARP #t) ;; Block comment begin
    (CR => CR #t) ;; CR LF
    (LF => LF)))

(define (ssplit-inblockcomment-parse-byte1 byte prev-sym)
  (define (others cls)
    (values 'OTHERS #f))
  (%dispatch prev-sym byte others
    (PIPE SHARP => BLOCK_COMMENT_END)
    (SHARP PIPE => BLOCK_COMMENT_BEGIN)
    (CR LF => CRLF)))

)
