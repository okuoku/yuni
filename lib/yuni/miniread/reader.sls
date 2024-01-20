(library (yuni miniread reader)
         (export
           utf8-read)
         (import (yuni scheme)
                 (yuni miniread reader-main))

;; TEMP

(define (%u8-list->bytevector lis)
  ;; FIXME: WHY?
  (define (itr bv cur rest)
    (when (pair? rest)
      (bytevector-u8-set! bv cur (car rest))
      (itr bv (+ cur 1) (cdr rest))))
  (let ((bv (make-bytevector (length lis))))
   (itr bv 0 lis)
   bv))

(define (%realize-string-raw bv start end)
  ;; FIXME: really confusing API..
  ;; R7RS
  (utf8->string bv start (+ 1 end)))

(define (%realize-string-fastpath bv start cur end)
  (cond
    ((= cur end)
     (%realize-string-raw bv (+ start 1) (- end 1)))
    ((= 92 (bytevector-u8-ref bv cur))
     #f)
    (else
      (%realize-string-fastpath bv start (+ 1 cur) end))))

(define (%realize-string-slowpath p in-escape? bv segstart cur end)
  ;; end points the last DQUOTE
  (cond
    ((> cur end)
     (error "Something wrong" segstart cur end))
    ((= cur end)
     ;; Flush current segment
     (write-string (%realize-string-raw bv segstart (- cur 1)) p)
     (get-output-string p))
    (in-escape?
      (let ((b (bytevector-u8-ref bv cur)))
       (case b
         ((97) ;; \a = 7
          (write-char (integer->char 7) p))
         ((98) ;; \b = 8
          (write-char (integer->char 8) p))
         ((116) ;; \t = 9
          (write-char (integer->char 9) p))
         ((110) ;; \n = #xa
          (write-char (integer->char #xa) p))
         ((100) ;; \d = #xd ????
          (write-char (integer->char #xd) p))
         ((34)  ;; \" = DQUOTE
          (write-char (integer->char 34) p))
         ((92) ;; \\ = BSLASH
          (write-char (integer->char 92) p))
         (else
           (error "Unknown escape in string" b))))
      (let ((next (+ cur 1)))
       (%realize-string-slowpath p #f bv next next end)))
    (else
      (cond
        ((= 92 (bytevector-u8-ref bv cur))
         ;; Flush current segment
         (write-string (%realize-string-raw bv segstart (- cur 1)) p)
         (let ((next (+ 1 cur)))
          (%realize-string-slowpath p #t bv next next end)))
        (else
          (%realize-string-slowpath p #f bv segstart (+ cur 1) end))))))

(define (%realize-string bv start end)
  (let ((s (%realize-string-fastpath bv start start end)))
   (or s
       ;; Exclude DQUOTE on the both ends
       (let ((start1 (+ start 1)))
         (%realize-string-slowpath (open-output-string) #f
                                   bv start1 start1 end)))))

(define (%realize-charlit bv start end) 
  (let* ((s (%realize-string-raw bv (+ start 2) end)))
   (cond
     ((= 1 (string-length s))
      (car (string->list s)))
     ((string=? "alarm" s) (integer->char 7))
     ((string=? "backspace" s) (integer->char 8))
     ((string=? "delete" s) (integer->char #x7f))
     ((string=? "escape" s) (integer->char #x1b))
     ((string=? "newline" s) (integer->char #xa))
     ((string=? "null" s) (integer->char 0))
     ((string=? "return" s) (integer->char #xd))
     ((string=? "space" s) (integer->char #x20))
     ((string=? "tab" s) (integer->char 9))
     (else
       (error "unknown char literal" s)))))

(define (%realize-number bv start end)
  (string->number (%realize-string-raw bv start end)))

(define (%realize-object bv start end)
  (string->symbol (%realize-string-raw bv start end)))

(define (%check-special-token bv start end) 
  ;; => VECTOR_BEGIN | BYTEVECTOR_BEGIN | IGNORE | NUMBER | #f
  (define s (%realize-string-raw bv start end))
  ;(write (list 'SPTKN s))(newline)
  (cond
    ((or (string=? "#vu8(" s) (string=? "#u8(" s))
     'BYTEVECTOR_BEGIN)
    ((string=? "#(" s)
     'VECTOR_BEGIN)
    (else 
      #|
      ;; Ignore every #! tokens
      (let ((head (and (<= 2 (string-length s))
                       (substring s 0 2))))
        (cond
          ((and head (string=? head "#!"))
           'IGNORE)
          ((and head (string=? head "#\\"))
           'CHARLIT)
          (else (and (string->number s) 'NUMBER))))
      |#
      (or (and (<= 2 (string-length s)) (char=? #\# (string-ref s 0))
               (let ((h (string-ref s 1)))
                 (or (and (char=? #\! h) 'IGNORE)
                     (and (char=? #\\ h) 'CHARLIT)
                     (and (char=? #\x h) 'NUMBER)
                     (and (char=? #\e h) 'NUMBER)
                     (and (char=? #\i h) 'NUMBER)
                     (and (char=? #\b h) 'NUMBER)
                     (and (char=? #\d h) 'NUMBER)
                     (and (char=? #\o h) 'NUMBER))))
          (and (<= 1 (string-length s)) 
               (case (string-ref s 0)
                 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                  'NUMBER)
                 (else #f)))
          (and (<= 2 (string-length s))
               (let ((h (string-ref s 0)))
                (or (and (char=? #\+ h) 'NUMBER)
                    (and (char=? #\- h) 'NUMBER))))))))

(define (%wrapobj mode obj)
  (case mode
    ((NEXT_QUOTE)
     (list 'quote obj))
    ((NEXT_QUASIQUOTE)
     (list 'quasiquote obj))
    ((NEXT_UNQUOTE)
     (list 'unquote obj))
    ((NEXT_UNQUOTE_SPLICING)
     (list 'unquote-splicing obj))
    ((NEXT_SYNTAX_QUOTE)
     (list 'syntax obj))
    ((NEXT_SYNTAX_UNQUOTE)
     (list 'unsyntax obj))
    ((NEXT_SYTNAX_UNQUOTE_SPLICING)
     (list 'unsyntax-splicing obj))
    ((NEXT_SYNTAX_QUASIQUOTE)
     (list 'quasisyntax obj))
    (else 
      (error "Unknown mode???" mode))))

(define (%realize bv lst) ;; => eof-object when comment only
  (define (elem-type e) (vector-ref e 0))
  (define (elem-start e) (vector-ref e 1))
  (define (elem-end e) (vector-ref e 2))
  (define (itr mode dump cur rest)
    (if (pair? rest)
      (let ((a (car rest))
            (d (cdr rest)))
        (let ((type (elem-type a))
              (start (elem-start a))
              (end (elem-end a)))
          (define (go-next mode obj cur dump)
            (define (next/pop take? obj)
              (let* ((x (car dump))
                     (prev-cur (car x))
                     (prev-mode (cdr x))
                     (next-dump (cdr dump)))
                (if take?
                    (go-next prev-mode obj prev-cur next-dump)
                    (itr prev-mode next-dump prev-cur d))))
            (define (nextnext x)
              (itr mode dump (cons x cur) d))
            (define (nextnext/pop obj)
              (next/pop #t obj))
            (define (nextdrop/pop) (next/pop #f #f))
            (define (nextchar/pop obj)
              (define c (string-ref obj 0))
              (nextnext/pop c))
            ;(display (list 'PACK: obj now-mode))(newline)
            ;; Apply NEXT-filters on cur
            (case mode
              ((NEXT_QUOTE NEXT_QUASIQUOTE NEXT_UNQUOTE
                           NEXT_UNQUOTE_SPLICING
                           NEXT_SYNTAX_QUOTE NEXT_SYNTAX_QUASIQUOTE 
                           NEXT_SYNTAX_UNQUOTE_SPLICING)
               (nextnext/pop (%wrapobj mode obj)))
              ((NEXT_DATUM_COMMENT)
               (nextdrop/pop))
              ((NEXT_CHAR_LITERAL)
               (nextchar/pop obj))
              (else (nextnext obj))))

          (define (single obj)
            (go-next mode obj cur dump))
          (define (single/pop obj)
            (let* ((x (car dump))
                   (next-dump (cdr dump))
                   (prev-cur (car x))
                   (prev-mode (cdr x)))
              (go-next prev-mode obj prev-cur next-dump)))

          (define (push-to-dump next-mode)
            ;; Push `(cons cur mode)` to `dump`
            (itr next-mode (cons (cons cur mode) dump) '() d))

          ;(display (list 'TKN: type start end))(newline)

          (case type
            ;; Comment (skip)
            ((COMMENT BLOCK_COMMENT) ;; FIXME: BLOCK_COMMENT ??
             (itr mode dump cur d))
            ;; Single
            ((TRUE) (single #t))
            ((FALSE) (single #f))
            ((STRING)
             (single (%realize-string bv start end)))
            ((OBJ)
             (let ((t (%check-special-token bv start end)))
              (case t
                ((VECTOR_BEGIN)
                 (push-to-dump 'VECTOR))
                ((BYTEVECTOR_BEGIN)
                 (push-to-dump 'BYTEVECTOR))
                ((IGNORE) 
                 (itr mode dump cur d))
                ((NUMBER)
                 (single (%realize-number bv start end)))
                ((CHARLIT)
                 (single (%realize-charlit bv start end)))
                (else 
                  (single (%realize-object bv start end))))))

            ;; 
            ((LIST_BEGIN_PAREN LIST_BEGIN_SQ)
             (push-to-dump 'LIST))

            ((TURN_TO_PAIR)
             (unless (eq? mode 'LIST)
               (error "Invalid dot in this context" mode))
             (itr 'PAIR dump cur d))

            ((LIST_END_PAREN LIST_END_SQ)
             ;; Realize object
             (let ((obj (case mode
                          ((LIST) (reverse cur))
                          ((PAIR)
                           (unless (and (pair? cur)
                                        (pair? (cdr cur)))
                             (error "Invalid object for pair" cur))
                           (let ((last (car cur))
                                 (second (cadr cur)))
                             (let loop ((rest (cddr cur))
                                        (cur (cons second last)))
                               (if (pair? rest)
                                 (loop (cdr rest) (cons (car rest) cur))
                                 cur))))
                          ((VECTOR) (list->vector (reverse cur)))
                          ((BYTEVECTOR) (%u8-list->bytevector (reverse cur)))
                          (else (error "unknown mode??" mode cur)))))
               (single/pop obj)))

            ((NEXT_QUOTE NEXT_QUASIQUOTE NEXT_UNQUOTE
              NEXT_UNQUOTE_SPLICING
              NEXT_SYNTAX_QUOTE NEXT_SYNTAX_QUASIQUOTE 
              NEXT_SYNTAX_UNQUOTE_SPLICING
              NEXT_DATUM_COMMENT NEXT_CHAR_LITERAL)
             (push-to-dump type))
            (else
              (error "unknown token type??" type)))))
      (case mode
        ((FIRST) (reverse cur))
        (else (error "unknown mode?" mode)))))
  (itr 'FIRST '() '() lst))
         
(define (%utf8-in bv)
  (define cur 0)
  (define len (bytevector-length bv))
  (lambda () ;; byte + stream + index
    (cond
      ((= cur len)
       (values (eof-object) bv cur))
      (else
        (let ((c cur)
              (b (bytevector-u8-ref bv cur)))
         (set! cur (+ 1 cur))
         (values b bv c))))))

(define (utf8-read bv) ;; => eof-object if comment-only
  (define tkn (make-tkn 128))
  (define cb (%utf8-in bv))
  (define mr (make-miniread))
  (define (capture tkn idx)
    (let ((start-index (tkn-start-index tkn idx))
          (end-index (tkn-end-index tkn idx))
          (type (tkn-type tkn idx)))
      (vector type start-index end-index)))
  (define (itr cur)
    (let ((r (miniread-main mr tkn 0 128 cb)))
     (cond
       ((eq? r #f)
        (%realize bv (reverse cur)))
       ((number? r)
        (let loop ((i 0)
                   (acc cur))
          (if (> i r)
              (itr acc)
              (loop (+ 1 i) (cons (capture tkn i) acc)))))
       (else
         (error "something wrong" r)))))
  (itr '()))

)
