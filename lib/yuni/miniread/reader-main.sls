;; FIXME: Take account hold-lineno hold-column!!
;; FIXME: Clean start/prev ASAP to lower GC load
(library (yuni miniread reader-main)
         (export
           make-miniread
           make-tkn
           tkn-start-stream
           tkn-start-index
           tkn-start-lineno
           tkn-start-column
           tkn-end-stream
           tkn-end-index
           tkn-end-lineno
           tkn-end-column
           tkn-type
           miniread-main)
         (import (yuni scheme)
                 (yuni core)
                 (yuni miniread charclasses)
                 (yuni miniread tokens))

(define* miniread ;; Reading context
  (state ;; #f
         ;; OBJ0 | OBJ1 | OBJ2
         ;; STRING0 | STRING1
         ;; LINECOMMENT0 | LINECOMMENT1
         ;; BLOCKCOMMENT0 | BLOCKCOMMENT1
   reg
   hold
   hold-stream
   hold-index
   lineno
   column
   blockcomment-depth
   start-stream
   start-index
   start-lineno
   start-column
   prev-type
   prev-stream
   prev-index
   prev-lineno
   prev-column))

(define* tokenvec-entry
  (start-stream
   start-index
   start-lineno
   start-column
   end-stream
   end-index
   end-lineno
   end-column
   type ;; OBJ
        ;; TRUE | FALSE
        ;; LIST_BEGIN_PAREN | LIST_END_PAREN | LIST_BEGIN_SQ | LIST_END_SQ
        ;; COMMENT | NEXT_DATUM_COMMENT | BLOCK_COMMENT
        ;; NEXT_QUOTE | NEXT_QUASIQUOTE | NEXT_UNQUOTE
        ;; NEXT_SYNTAX_QUOTE | NEXT_SYNTAX_QUASIQUOTE | NEXT_UNQUOTE_SPLICING
        ;; STRING
    ))

(define (make-tkn num)
  (list->vector 
    (map (lambda (e) (make tokenvec-entry))
         (vector->list (make-vector num)))))

(define (%tref vec idx sym)
  (~ (vector-ref vec idx) sym))

(define (tkn-start-stream vec idx)
  (%tref vec idx 'start-stream))
(define (tkn-start-index vec idx)
  (%tref vec idx 'start-index))
(define (tkn-start-lineno vec idx)
  (%tref vec idx 'start-lineno))
(define (tkn-start-column vec idx)
  (%tref vec idx 'start-column))

(define (tkn-end-stream vec idx)
  (%tref vec idx 'end-stream))
(define (tkn-end-index vec idx)
  (%tref vec idx 'end-index))
(define (tkn-end-lineno vec idx)
  (%tref vec idx 'end-lineno))
(define (tkn-end-column vec idx)
  (%tref vec idx 'end-column))

(define (tkn-type vec idx)
  (%tref vec idx 'type))

(define (make-miniread)
  (make miniread
        (blockcomment-depth 0)
        (hold #f)
        (state #f)))

(define (miniread-main mr vec vecidx vecend cb) ;; => filled idx / #f
  (define terminate? #f)
  (define curidx vecidx)
  (define retidx #f)
  (define (state) (~ mr 'state))
  (define (set-state! st) (~ mr 'state := st))
  (define (blockcomment-depth-zero?)
    (zero? (~ mr 'blockcomment-depth)))
  (define (%blockcomment-depth-add! x)
    (let ((d (~ mr 'blockcomment-depth)))
     (~ mr 'blockcomment-depth := (+ x d))))
  (define (blockcomment-depth++) (%blockcomment-depth-add! 1))
  (define (blockcomment-depth--) (%blockcomment-depth-add! -1))

  ;; Step dispatch
  (define (callstep next b stream index)
    (define (step type has-next?)
      (define prev-type (~ mr 'reg))
      (~ mr 'reg := type)
      (next b (state) prev-type type has-next? stream index))
    (define (dostep0 p) (call-with-values (lambda () (p b)) step))
    (define (dostep p) (call-with-values (lambda () (p b (~ mr 'reg))) step))

    (case (state)
      ((CHARLIT) 
       ;; Special: Character literal
       (next b 'CHARLIT #f 'CHARLIT #f stream index))
      ((#f OBJ0 OBJ0/SHARP)
       (dostep0 ssplit-parse-byte0))
      ((OBJ1 OBJ1/SHARP)
       (dostep ssplit-parse-byte1))
      ((OBJ2)
       (dostep ssplit-parse-byte2))
      ((STRING0)
       (dostep0 ssplit-instring-parse-byte0))
      ((STRING1)
       (dostep ssplit-instring-parse-byte1))
      ((LINECOMMENT0)
       (dostep0 ssplit-incomment-parse-byte0))
      ((LINECOMMENT1)
       (dostep ssplit-incomment-parse-byte1))
      ((BLOCKCOMMENT0)
       (dostep0 ssplit-inblockcomment-parse-byte0))
      ((BLOCKCOMMENT1)
       (dostep ssplit-inblockcomment-parse-byte1))
      (else (error "unexpected state" (state)))))

  ;; Events
  (define (char b st prev-type type has-next? stream index)
    (define (whitespace?) (ssplit-byte-whitespace? b))
    (define (delimiter?) (ssplit-byte-delimiter? b))
    (define (paren-l?) (eq? 'PAREN_L (ssplit-byte-class b)))
    (define (hold) 
      (~ mr 'hold := b)
      (~ mr 'hold-index := index)
      (~ mr 'hold-stream := stream))
    (define (set-prev-here type)
      (~ mr 'prev-type := type)
      (~ mr 'prev-stream := stream)
      (~ mr 'prev-index := index)
      (~ mr 'prev-lineno := (~ mr 'lineno))
      (~ mr 'prev-column := (~ mr 'column)))

    (define (begin-here next-state)
      (let ((st (state)))
       ;; Sanity check: Disposable states are #f / CHARLIT
       (when (and st (not (eq? st 'CHARLIT)))
         (error "Invalid state at begin-here" st)) )
      (~ mr 'state := next-state)
      (~ mr 'start-stream := stream)
      (~ mr 'start-index := index)
      (~ mr 'start-lineno := (~ mr 'lineno))
      (~ mr 'start-column := (~ mr 'column)))

    (define (%tkn-set-start! tkn type) ;; for both here/prev
       (~ tkn 'type := type) 
       (~ tkn 'start-stream := (~ mr 'start-stream))
       (~ tkn 'start-index := (~ mr 'start-index))
       (~ tkn 'start-lineno := (~ mr 'start-lineno))
       (~ tkn 'start-column := (~ mr 'start-column)))

    (define (%emit-tkn!)
      ;; Reset state to #f
      (~ mr 'state := #f)
      (set! retidx curidx)
      (set! curidx (+ 1 curidx)))

    (define (end-here tkn-type)
      ;; Fill a token
      (let ((tkn (vector-ref vec curidx)))
       (%tkn-set-start! tkn tkn-type)
       (~ tkn 'end-stream := stream)
       (~ tkn 'end-index := index)
       (~ tkn 'end-lineno := (~ mr 'lineno))
       (~ tkn 'end-column := (~ mr 'column)))
      (%emit-tkn!))

    (define (end-prev)
      (let ((tkn (vector-ref vec curidx)))
       (%tkn-set-start! tkn (~ mr 'prev-type))
       (~ tkn 'end-stream := (~ mr 'prev-stream))
       (~ tkn 'end-index := (~ mr 'prev-index))
       (~ tkn 'end-lineno := (~ mr 'prev-lineno))
       (~ tkn 'end-column := (~ mr 'prev-column)))
      (%emit-tkn!))

    (define (tkn-single tkn-type)
      (begin-here #f)
      (end-here tkn-type))

    ;(write (list 'CHAR: (integer->char b) st prev-type type has-next?))(newline)

    (case st
      ((CHARLIT) ;; Special: Character literal
       ;; FIXME: Filter-out whitespaces here
       (set-prev-here 'OBJ)
       (set-state! 'OBJ0))
      ((#f)
       (case type
         ((;; Lists
           LIST_BEGIN_PAREN LIST_END_PAREN LIST_BEGIN_SQ LIST_END_SQ
           ;; Quotes
           NEXT_QUOTE NEXT_QUASIQUOTE)
          (tkn-single type))
         ((SEMICOLON)
          (begin-here 'LINECOMMENT0))
         ((DQUOTE)
          (begin-here 'STRING0))
         ((NEXT_UNQUOTE)
          (set-prev-here 'NEXT_UNQUOTE)
          (begin-here 'OBJ1))
         ((SPACE CR LF)
           ;; Space. Ignore.
           'do-nothing)
         ((SHARP)
          (set-prev-here 'OBJ)
          (begin-here 'OBJ1/SHARP))
         (else ;; Some object
           ;; OBJ0 is for triggering ssplit-parse-byte0
           ;; which will accept delimiters.
           (set-prev-here 'OBJ)
           (if has-next?
             (begin-here 'OBJ1)
             (begin-here 'OBJ0)))))

      ((OBJ0 OBJ0/SHARP)
       (case type
         ((LIST_BEGIN_PAREN) ;; Include L paren into token
          (cond
            ((eq? st 'OBJ0/SHARP)
             (end-here 'OBJ))
            (else
              (end-prev)
              (hold)
              (set-state! #f))))
         (else
           (cond
             ((or (whitespace?) (delimiter?))
              (end-prev)
              (hold)
              (set-state! #f))
             (else
               (set-prev-here 'OBJ)
               (when has-next?
                 (set-state! 'OBJ1)))))))

      ((OBJ1 OBJ1/SHARP)
       (case type
         ;; FIXME: We never receive CRLF here.
         ((NEXT_CHAR_LITERAL)
          (set-prev-here 'OBJ)
          (set-state! 'CHARLIT))
         ((NEXT_SYNTAX_QUOTE NEXT_SYNTAX_QUASIQUOTE NEXT_UNQUOTE_SPLICING
           NEXT_DATUM_COMMENT
           TRUE FALSE)
          (end-here type))
         ((BLOCK_COMMENT_BEGIN)
          (blockcomment-depth++)
          (set-state! 'BLOCKCOMMENT0))
         (else
           (case prev-type ;; follow-up OBJ0
             ((NEXT_UNQUOTE)
              (end-prev)
              (hold)
              (set-state! #f))
             (else
               ;; Hold a byte and restart with #f/OBJ0
               (cond
                 ((and (eq? st 'OBJ1/SHARP) (paren-l?))
                  ;; Include L paren into token
                  (end-here 'OBJ))
                 ((delimiter?)
                  (end-prev)
                  (hold)
                  (set-state! #f))
                 (else
                   (hold)
                   (if (eq? st 'OBJ1/SHARP)
                     (set-state! 'OBJ0/SHARP)
                     (set-state! 'OBJ0)))))))))
      ((OBJ2)
       (case type
         ((NEXT_SYNTAX_UNQUOTE_SPLICING)
          (end-here type))
         (else
           (case prev-type
             ((NEXT_SYNTAX_UNQUOTE)
              (end-prev)
              (hold)
              (set-state! #f))
             (else (error "???"))))))
      ((STRING0)
       (case type
         ((BACKSLASH)
          (set-state! 'STRING1))
         ((DQUOTE)
          (end-here 'STRING))
         (else 'do-nothing)))
      ((STRING1)
       (set-state! 'STRING0))

      ((LINECOMMENT0)
       (case type
         ((LF)
          (end-here 'COMMENT))
         ((CR)
          ;; Wait for CRLF.
          (set-prev-here 'COMMENT)
          (set-state! 'LINECOMMENT1))))
      ((LINECOMMENT1)
       (case type
         ((CRLF)
          (end-here 'COMMENT))
         (else
           (end-prev)
           (hold)
           (set-state! #f))))

      ((BLOCKCOMMENT0)
       (case type
         ((PIPE SHARP)
          (set-state! 'BLOCKCOMMENT1))
         (else 'do-nothing)))
      ((BLOCKCOMMENT1)
       (case type
         ((BLOCK_COMMENT_BEGIN)
          (blockcomment-depth++))
         ((BLOCK_COMMENT_END)
          (blockcomment-depth--)
          (cond
            ((blockcomment-depth-zero?)
             (end-here 'COMMENT)
             (set-state! #f))
            (else
              (set-state! 'BLOCKCOMMENT0))))
         (else (set-state! 'BLOCKCOMMENT0))
         ))
      (else (error "?????"))))

  (define (stream-end stream index)
    ;; FIXME: Inject whitespace
    (callstep char #x20 #f #f)
    (set! terminate? #t))

  ;; Driver
  (define (byte b stream index)
    (cond
      ((eof-object? b) 
       ;; stream-end symbol
       (stream-end stream index))
      (else
        (unless (integer? b) (error "Invalid value" b))
        (callstep char b stream index))))

  (define (itr)
    (unless (or terminate? (= curidx vecend))
      (let ((hold (~ mr 'hold)))
       (if hold
         (let ((stream (~ mr 'hold-stream))
               (index (~ mr 'hold-index)))
           ;; Consume a byte from hold register
           (~ mr 'hold := #f)
           (byte hold stream index))
         (call-with-values cb byte)))
      (itr)))
  (itr)
  retidx)

)
