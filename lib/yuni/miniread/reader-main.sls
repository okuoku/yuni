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
                 (yuni miniread charclasses)
                 (yuni miniread tokens))

         
;; FIXME: Use our own record system for now...

(define (make-mr) (make-vector 17))
(define (mr-state mr)              (vector-ref mr 0))
(define (mr-reg mr)                (vector-ref mr 1))
(define (mr-hold mr)               (vector-ref mr 2))
(define (mr-hold-stream mr)        (vector-ref mr 3))
(define (mr-hold-index mr)         (vector-ref mr 4))
(define (mr-lineno mr)             (vector-ref mr 5))
(define (mr-column mr)             (vector-ref mr 6))
(define (mr-blockcomment-depth mr) (vector-ref mr 7))
(define (mr-start-stream mr)       (vector-ref mr 8))
(define (mr-start-index mr)        (vector-ref mr 9))
(define (mr-start-lineno mr)       (vector-ref mr 10))
(define (mr-start-column mr)       (vector-ref mr 11))
(define (mr-prev-type mr)          (vector-ref mr 12))
(define (mr-prev-stream mr)        (vector-ref mr 13))
(define (mr-prev-index mr)         (vector-ref mr 14))
(define (mr-prev-lineno mr)        (vector-ref mr 15))
(define (mr-prev-column mr)        (vector-ref mr 16))
(define (mr-state! mr v)              (vector-set! mr 0 v))
(define (mr-reg! mr v)                (vector-set! mr 1 v))
(define (mr-hold! mr v)               (vector-set! mr 2 v))
(define (mr-hold-stream! mr v)        (vector-set! mr 3 v))
(define (mr-hold-index! mr v)         (vector-set! mr 4 v))
(define (mr-lineno! mr v)             (vector-set! mr 5 v))
(define (mr-column! mr v)             (vector-set! mr 6 v))
(define (mr-blockcomment-depth! mr v) (vector-set! mr 7 v))
(define (mr-start-stream! mr v)       (vector-set! mr 8 v))
(define (mr-start-index! mr v)        (vector-set! mr 9 v))
(define (mr-start-lineno! mr v)       (vector-set! mr 10 v))
(define (mr-start-column! mr v)       (vector-set! mr 11 v))
(define (mr-prev-type! mr v)          (vector-set! mr 12 v))
(define (mr-prev-stream! mr v)        (vector-set! mr 13 v))
(define (mr-prev-index! mr v)         (vector-set! mr 14 v))
(define (mr-prev-lineno! mr v)        (vector-set! mr 15 v))
(define (mr-prev-column! mr v)        (vector-set! mr 16 v))

(define (make-tve) (make-vector 9))
(define (tve-start-stream tve) (vector-ref tve 0))
(define (tve-start-index tve)  (vector-ref tve 1))
(define (tve-start-lineno tve) (vector-ref tve 2))
(define (tve-start-column tve) (vector-ref tve 3))
(define (tve-end-stream tve)   (vector-ref tve 4))
(define (tve-end-index tve)    (vector-ref tve 5))
(define (tve-end-lineno tve)   (vector-ref tve 6))
(define (tve-end-column tve)   (vector-ref tve 7))
(define (tve-type tve)         (vector-ref tve 8))
(define (tve-start-stream! tve v)  (vector-set! tve 0 v))
(define (tve-start-index! tve v)   (vector-set! tve 1 v))
(define (tve-start-lineno! tve v)  (vector-set! tve 2 v))
(define (tve-start-column! tve v)  (vector-set! tve 3 v))
(define (tve-end-stream! tve v)    (vector-set! tve 4 v))
(define (tve-end-index! tve v)     (vector-set! tve 5 v))
(define (tve-end-lineno! tve v)    (vector-set! tve 6 v))
(define (tve-end-column! tve v)    (vector-set! tve 7 v))
(define (tve-type! tve v)          (vector-set! tve 8 v))

(define (make-tkn num)
  (let ((v (make-vector num)))
   (let loop ((pos 0))
    (unless (= pos num)
      (vector-set! v pos (make-tve))
      (loop (+ 1 pos))))
   v))

(define (%tref vec idx proc)
  (proc (vector-ref vec idx)))

(define (tkn-start-stream vec idx)
  (%tref vec idx tve-start-stream))
(define (tkn-start-index vec idx)
  (%tref vec idx tve-start-index))
(define (tkn-start-lineno vec idx)
  (%tref vec idx tve-start-lineno))
(define (tkn-start-column vec idx)
  (%tref vec idx tve-start-column))

(define (tkn-end-stream vec idx)
  (%tref vec idx tve-end-stream))
(define (tkn-end-index vec idx)
  (%tref vec idx tve-end-index))
(define (tkn-end-lineno vec idx)
  (%tref vec idx tve-end-lineno))
(define (tkn-end-column vec idx)
  (%tref vec idx tve-end-column))

(define (tkn-type vec idx)
  (%tref vec idx tve-type))

(define (make-miniread)
  (let ((out (make-mr)))
   (mr-blockcomment-depth! out 0)
   (mr-hold! out #f)
   (mr-state! out #f)
   out))

(define (miniread-main mr vec vecidx vecend cb) ;; => filled idx / #f
  (define terminate? #f)
  (define curidx vecidx)
  (define retidx #f)
  (define (state) (mr-state mr))
  (define (set-state! st) (mr-state! mr st))
  (define (blockcomment-depth-zero?)
    (zero? (mr-blockcomment-depth mr)))
  (define (%blockcomment-depth-add! x)
    (let ((d (mr-blockcomment-depth mr)))
     (mr-blockcomment-depth! mr (+ x d))))
  (define (blockcomment-depth++) (%blockcomment-depth-add! 1))
  (define (blockcomment-depth--) (%blockcomment-depth-add! -1))

  ;; Step dispatch
  (define step-b #f)
  (define step-next #f)
  (define step-stream #f)
  (define step-index #f)
  (define (step type has-next?)
    (let ((prev-type (mr-reg mr)))
     (mr-reg! mr type)
     (step-next step-b (state) prev-type type has-next? 
                step-stream step-index)))
  (define (dostep0 p) (call-with-values (lambda () (p step-b)) step))
  (define (dostep p) (call-with-values (lambda () (p step-b (mr-reg mr))) step))

  (define (callstep next b stream index)
    (set! step-b b)
    (set! step-stream stream)
    (set! step-index index)
    (set! step-next next)

    (case (state)
      ((CHARLIT) 
       ;; Special: Character literal
       (next b 'CHARLIT #f 'CHARLIT #f stream index))
      ((#f OBJ0 OBJ0/SHARP OBJ0/DOT)
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

  ;; registers
  (define cur-b #f)
  (define cur-stream #f)
  (define cur-index #f)

  (define (whitespace?) (ssplit-byte-whitespace? cur-b))
  (define (delimiter?) (ssplit-byte-delimiter? cur-b))
  (define (paren-l?) (eq? 'PAREN_L (ssplit-byte-class cur-b)))
  (define (hold) 
    (mr-hold! mr cur-b)
    (mr-hold-index! mr cur-index)
    (mr-hold-stream! mr cur-stream))
  (define (set-prev-here type)
    (mr-prev-type! mr type)
    (mr-prev-stream! mr cur-stream)
    (mr-prev-index! mr cur-index)
    (mr-prev-lineno! mr (mr-lineno mr))
    (mr-prev-column! mr (mr-column mr)))

  (define (begin-here next-state)
    (let ((st (state)))
      ;; Sanity check: Disposable states are #f / CHARLIT
      (when (and st (not (eq? st 'CHARLIT)))
        (error "Invalid state at begin-here" st)) )
    (mr-state! mr next-state)
    (mr-start-stream! mr cur-stream)
    (mr-start-index! mr cur-index)
    (mr-start-lineno! mr (mr-lineno mr))
    (mr-start-column! mr (mr-column mr)))

  (define (%tkn-set-start! tkn type) ;; for both here/prev
    (tve-type! tkn type) 
    (tve-start-stream! tkn (mr-start-stream mr))
    (tve-start-index! tkn (mr-start-index mr))
    (tve-start-lineno! tkn (mr-start-lineno mr))
    (tve-start-column! tkn (mr-start-column mr)))

  (define (%emit-tkn!)
    ;; Reset state to #f
    (mr-state! mr #f)
    (set! retidx curidx)
    (set! curidx (+ 1 curidx)))

  (define (end-here tkn-type)
    ;; Fill a token
    (let ((tkn (vector-ref vec curidx)))
      (%tkn-set-start! tkn tkn-type)
      (tve-end-stream! tkn cur-stream)
      (tve-end-index! tkn cur-index)
      (tve-end-lineno! tkn (mr-lineno mr))
      (tve-end-column! tkn (mr-column mr)))
    (%emit-tkn!))

  (define (end-prev)
    (let ((tkn (vector-ref vec curidx)))
      (%tkn-set-start! tkn (mr-prev-type mr))
      (tve-end-stream! tkn (mr-prev-stream mr))
      (tve-end-index! tkn (mr-prev-index mr))
      (tve-end-lineno! tkn (mr-prev-lineno mr))
      (tve-end-column! tkn (mr-prev-column mr)))
    (%emit-tkn!))

  (define (tkn-single tkn-type)
    (begin-here #f)
    (end-here tkn-type))

  ;; Events
  (define (char b st prev-type type has-next? stream index)
    (set! cur-b b)
    (set! cur-stream stream)
    (set! cur-index index)

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
         ((DOT)
          (set-prev-here 'TURN_TO_PAIR)
          (begin-here 'OBJ0/DOT))
         (else ;; Some object
           ;; OBJ0 is for triggering ssplit-parse-byte0
           ;; which will accept delimiters.
           (set-prev-here 'OBJ)
           (if has-next?
             (begin-here 'OBJ1)
             (begin-here 'OBJ0)))))

      ((OBJ0 OBJ0/SHARP OBJ0/DOT)
       (case type
         ((LIST_BEGIN_PAREN) ;; Include L paren into token
          (case st
            ((OBJ0/SHARP)
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
    (callstep char 32 #f #f)
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
      (let ((hold (mr-hold mr)))
       (if hold
         (let ((stream (mr-hold-stream mr))
               (index (mr-hold-index mr)))
           ;; Consume a byte from hold register
           (mr-hold! mr #f)
           (byte hold stream index))
         (call-with-values cb byte)))
      (itr)))
  (itr)
  retidx)

)
