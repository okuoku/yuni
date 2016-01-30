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
  ;; R7RS
  (utf8->string bv start (+ 1 end)))

(define (%realize-PLACEHOLDER bv start end)
  (let ((s (%realize-string-raw bv start end)))
   (read (open-input-string s))))

(define %realize-string %realize-PLACEHOLDER)
(define %realize-charlit %realize-PLACEHOLDER)

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
      ;; Ignore every #! tokens
      (let ((head (and (<= 2 (string-length s))
                       (substring s 0 2))))
        (cond
          ((and head (string=? head "#!"))
           'IGNORE)
          ((and head (string=? head "#\\"))
           'CHARLIT)
          (else (and (string->number s) 'NUMBER)))))))

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
                (itr prev-mode next-dump 
                     (if take?  (cons obj prev-cur) prev-cur) d)))
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
              ((NEXT_QUOTE)
               (nextnext/pop (list 'quote obj)))
              ((NEXT_QUASIQUOTE)
               (nextnext/pop (list 'quasiquote obj)))
              ((NEXT_UNQUOTE)
               (nextnext/pop (list 'unquote obj)))
              ((NEXT_UNQUOTE_SPLICING)
               (nextnext/pop (list 'unquote-splicing obj)))
              ((NEXT_SYNTAX_QUOTE)
               (nextnext/pop (list 'syntax obj)))
              ((NEXT_SYNTAX_UNQUOTE)
               (nextnext/pop (list 'unsyntax obj)))
              ((NEXT_SYTNAX_UNQUOTE_SPLICING)
               (nextnext/pop (list 'unsyntax-splicing obj)))
              ((NEXT_SYNTAX_QUASIQUOTE)
               (nextnext/pop (list 'quasisyntax obj)))
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
            ((COMMENT BLOCK_COMMENT)
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

            ((LIST_END_PAREN LIST_END_SQ)
             ;; Realize object
             (let ((obj (case mode
                          ((LIST) (reverse cur))
                          ((VECTOR) (list->vector (reverse cur)))
                          ((BYTEVECTOR) (%u8-list->bytevector (reverse cur)))
                          (else (error "unknown mode??" mode)))))
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
  (define tkn (make-tkn 1))
  (define cb (%utf8-in bv))
  (define mr (make-miniread))
  (define (capture tkn idx)
    (let ((start-index (tkn-start-index tkn idx))
          (end-index (tkn-end-index tkn idx))
          (type (tkn-type tkn idx)))
      (vector type start-index end-index)))
  (define (itr cur)
    (let ((r (miniread-main mr tkn 0 1 cb)))
     (cond
       ((eq? r #f)
        (%realize bv (reverse cur)))
       ((and (number? r) (= r 0))
        (itr (cons (capture tkn 0) cur)))
       (else
         (error "something wrong" r)))))
  (itr '()))

)
