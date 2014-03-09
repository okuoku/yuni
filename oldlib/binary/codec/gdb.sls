(library (yuni binary codec gdb)
         (export make-gdb-talker/target
                 make-gdb-talker/host
                 gdb-reply-ok
                 gdb-reply-signal
                 gdb-reply-exit-status
                 gdb-reply-trap
                 gdb-reply-error
                 gdb-reply-registers
                 gdb-reply-memory
                 gdb-reply-threadinfo/id
                 gdb-reply-threadinfo/terminate
                 gdb-reply-current-thread)
         (import (rnrs) 
                 (match)
                 (mosh pp)
                 (rnrs r5rs)
                 (srfi :8)
                 (srfi :42))

;;; GDB Packets

(define-syntax ascii
  (syntax-rules ()
    ((_ x) (char->integer x)))) 

(define (nibble i)
  (case i
    ((0) (ascii #\0))
    ((1) (ascii #\1))
    ((2) (ascii #\2))
    ((3) (ascii #\3))
    ((4) (ascii #\4))
    ((5) (ascii #\5))
    ((6) (ascii #\6))
    ((7) (ascii #\7))
    ((8) (ascii #\8))
    ((9) (ascii #\9))
    ((10) (ascii #\A))
    ((11) (ascii #\B))
    ((12) (ascii #\C))
    ((13) (ascii #\D))
    ((14) (ascii #\E))
    ((15) (ascii #\F))))

;; FIXME
(define (nibble-num c)
  (cond
    ((= c (ascii #\0)) 0)
    ((= c (ascii #\1)) 1)
    ((= c (ascii #\2)) 2)
    ((= c (ascii #\3)) 3)
    ((= c (ascii #\4)) 4)
    ((= c (ascii #\5)) 5)
    ((= c (ascii #\6)) 6)
    ((= c (ascii #\7)) 7)
    ((= c (ascii #\8)) 8)
    ((= c (ascii #\9)) 9)
    ((or (= c (ascii #\A)) (= c (ascii #\a))) 10)
    ((or (= c (ascii #\B)) (= c (ascii #\b))) 11)
    ((or (= c (ascii #\C)) (= c (ascii #\c))) 12)
    ((or (= c (ascii #\D)) (= c (ascii #\d))) 13)
    ((or (= c (ascii #\E)) (= c (ascii #\e))) 14)
    ((or (= c (ascii #\F)) (= c (ascii #\f))) 15)
    (else #f)))

(define (set-nibble-hex! bv off i)
  (bytevector-u8-set! bv off (nibble i)))

(define (set-u8-hex! bv off i)
  (let ((up (quotient i 16))
        (down (remainder i 16)))
    (set-nibble-hex! bv off up)
    (set-nibble-hex! bv (+ 1 off) down)))

;;; Packet Chunker

(define START-PACKET (ascii #\$))
(define END-PACKET (ascii #\#))
(define ESCAPE (ascii #\}))
(define STAR (ascii #\*))
(define COLON (ascii #\:))
(define SEMICOLON (ascii #\;))
(define COMMA (ascii #\,))

(define BREAK 3)

(define ACK (ascii #\+))
(define NACK (ascii #\-))
(define MINUS (ascii #\-))

(define (bv-byte b)
  (let ((bv (make-bytevector 1)))
    (bytevector-u8-set! bv 0 b)
    bv))

(define ACK-packet (bv-byte ACK))
(define NACK-packet (bv-byte NACK))

(define (make-gdb-packet-chunker unrle? callback) ;; (^[byte])
  ;; callback <= #t (break) / #f (checksum error) / bytevector
  (define pktbuf-len)
  (define pktbuf)
  (define p)
  (define sum)
  (define sumr)
  (define state)
  (define lastbyte #f)
  (define (setpktbuf size)
    (let ((bv (make-bytevector size)))
      (bytevector-copy! pktbuf 0 bv 0 pktbuf-len)
      (set! pktbuf bv)
      (set! pktbuf-len size)))

  (define (packet len)
    (let ((bv (make-bytevector len)))
      (bytevector-copy! pktbuf 0 bv 0 len)
      bv))
  (define (setbyte c)
    (set! lastbyte c)
    (when (= pktbuf-len p)
      (setpktbuf (+ p 1000))) 
    (bytevector-u8-set! pktbuf p c)
    (set! p (+ 1 p)))
  (define (resetbuf)
    (set! p 0)
    (set! sum 0))

  (resetbuf)
  (set! pktbuf-len 1000)
  (set! pktbuf (make-bytevector pktbuf-len))
  (set! state #f)
  (lambda (byte)
    ;; Receive $DDDD...#CC sequence
    (case state
      ((#f)
       ;; FIXME: handle ^C
       ;; Ignore non packet strings
       (cond
         ((= byte BREAK)
          (callback #t))
         ((= byte START-PACKET) 
          (set! state #t))))
      ((#t escape rle-count)
       (cond
         ((= byte END-PACKET)
          (set! state 'checksum0))
         (else
           (cond
             ((eq? state 'rle-count)
              (do-ec (: i (- byte 29))
                     (setbyte lastbyte))
              (set! state '#t))
             ((= byte ESCAPE)
              (set! state 'escape))
             ((and unrle? (not (eq? state 'escape)) (= byte STAR))
              (set! state 'rle-count))
             (else
               (setbyte (if (eq? state 'escape) 
                          (bitwise-xor byte #x20) 
                          byte))
               (set! state #t)))
           ;; We have to include ESCAPE char to sum
           ;; and don't have to unescape 
           (set! sum (+ sum byte)) 
           (when (> sum 255)
             (set! sum (- sum 256))))))
      ((checksum0)
       (set! state 'checksum1)
       (set! sumr (nibble-num byte)))
      ((checksum1)
       (set! state #f)
       (let ((checksum (+ (* sumr 16) (nibble-num byte))))
         (let ((len p)
               (thispacketsum sum))
           (resetbuf)
           #|
           (display (list 'packet: (utf8->string (packet len)) 
                          'sum: checksum thispacketsum ))(newline)
           |#
           (callback (and (= checksum thispacketsum)
                          (packet len)))))))))

;;; Escape
(define (escape-byte? c)
  (or (= START-PACKET c) (= END-PACKET c) 
      (= ESCAPE c) (= STAR c)))

(define (escape/checksum bv) ;; => bv
  (define sum 0)
  (define in-len (bytevector-length bv))
  (define len 4) ;; START + END + CHECKSUM(2bytes)
  (do-ec (: i in-len)
         (let ((c (bytevector-u8-ref bv i)))
           (cond
             ((escape-byte? c)
              (set! len (+ len 1))))))
  (set! len (+ in-len len))
  (let ((out (make-bytevector len)))
    (define (itr off idx)
      (define (update c)
        (bytevector-u8-set! out off c)
        (set! sum (+ sum c))
        (when (> sum 255)
          (set! sum (- sum 256)) ))
      (if (= idx in-len)
        off
        (let ((c (bytevector-u8-ref bv idx)))
          (cond
            ((escape-byte? c)
             (update out off ESCAPE)
             (update out (+ off 1) (bitwise-xor c #x20))
             (itr (+ off 2) (+ idx 1)))
            (else
              (update c)
              (itr (+ off 1) (+ idx 1)))))))
    (bytevector-u8-set! out 0 START-PACKET)
    (let ((next (itr 1 0)))
      (bytevector-u8-set! out next END-PACKET)
      (set-u8-hex! out (+ next 1) sum)) 
    (set! sum 0)
    out))

(define (format-memory bv)
  (let ((out (make-bytevector (* 2 (bytevector-length bv)))))
    (fill-hex out 0 bv)
    out))

(define (fill-hex out off in)
  (do-ec (: i (bytevector-length in))
         (set-u8-hex! out (+ off (* 2 i))
                      (bytevector-u8-ref in i))))

(define (gdb-reply-registers bv)
  (let ((out (make-bytevector (* 2 (bytevector-length bv)))))
    (fill-hex out 0 bv) 
    ;(display (list 'from: bv))(newline)
    ;(display (list 'to: (utf8->string out)))(newline)
    (escape/checksum out)))

(define gdb-reply-memory gdb-reply-registers)

(define (gdb-reply-char ch)
  (let ((in (make-bytevector 1)))
    (bytevector-u8-set! in 0 (ascii ch))
    (escape/checksum in)))

(define (gdb-reply-letter ch val)
  (let ((in (make-bytevector 3)))
    (bytevector-u8-set! in 0 (ascii ch))
    (set-u8-hex! in 1 val)
    (escape/checksum in)))

(define (gdb-reply-ok)
  (escape/checksum (string->utf8 "OK")))

(define (gdb-reply-signal sig)
  (gdb-reply-letter #\S sig))

(define (gdb-reply-exit-status e)
  (gdb-reply-letter #\W e))

(define (gdb-reply-trap e)
  (gdb-reply-letter #\T e))

(define (gdb-reply-error e)
  (gdb-reply-letter #\E e))

(define (gdb-reply-threadinfo/terminate)
  (gdb-reply-char #\l))

(define (bv-concat . bv)
  (define total-len (fold-left + 0 (map bytevector-length bv)))
  (define out (make-bytevector total-len))
  (fold-left (lambda (pos bv)
               (let ((len (bytevector-length bv)))
                 (bytevector-copy! bv 0 out pos len) 
                 (+ pos len)))
             0
             bv)
  out)

(define (format-value val)
  (define (gen-base16 v)
    (define (itr acc cur)
      (if (= acc 0) 
        cur
        (itr (quotient acc 16)
             (cons (remainder acc 16) cur))))
    (itr v '()))
  (let ((o (gen-base16 val)))
    (u8-list->bytevector
    (if (null? o)
      (list (ascii #\0))
      (map nibble o)))))

(define (gdb-reply-threadinfo/id id)
  (escape/checksum
    (bv-concat
      (u8-list->bytevector (list (ascii #\m)))
      (format-value id))))

(define (gdb-reply-current-thread id)
  (escape/checksum
    (bv-concat
      (u8-list->bytevector (list (ascii #\Q)
                                 (ascii #\C)))
      (format-value id))))

;; Scan gdb queries.
;; fomart* = COMMA | SEMICOLON | COLON | MEMORY | VALUE
(define (gdb-scan bv off format)
  (define neg? #f)
  (define len (bytevector-length bv))
  (define (itr acc fmt sep off) ;; => value off
    ;; Read to sep, 
    (define byte (if (= len off) 'INVALID-VALUE (bytevector-u8-ref bv off)))
    (define (seperator?)
      (case sep
        ((#f) (= len off))
        ((COMMA) (= COMMA byte))
        ((SEMICOLON) (= SEMICOLON byte))
        ((COLON) (= COLON byte))
        (else (error 'itr "Invalid sep" sep))))
    (define (value)
      (case fmt
        ((MEMORY) (u8-list->bytevector (reverse acc)))
        ((VALUE) (if neg?  (- acc) acc))
        (else (assert #f))))
    (define (update)
      (case fmt
        ((MEMORY)
         (itr (cons (+ (* 16 (nibble-num byte))
                       (nibble-num (bytevector-u8-ref bv (+ off 1)))) acc) 
              fmt sep (+ off 2)))
        ((VALUE)
         (if (= byte MINUS)
           (begin 
             (set! neg? #t)
             (itr acc fmt sep (+ off 1)))
           (itr (+ (nibble-num byte) (* acc 16)) fmt sep (+ off 1))))
        (else (error 'itr "Invalid fmt" fmt))))
    (if (seperator?)
      (values (value) (+ off 1))
      (update)))

  (define (next off fmt cur)
    (if (pair? fmt)
      (let ((a (car fmt))
            (d (cdr fmt)))
        (let ((sep (if (pair? d) (car d) #f))
              (n (if (pair? d) (cdr d) d)))
          (receive (val off) (itr (case a
                                    ((VALUE) 0)
                                    ((MEMORY) '())) a sep off)
            (next off n (cons val cur)))))
      cur))
  (reverse (next off format '())))

(define-syntax dispatch-topchar
  (syntax-rules ()
    ((_ obj (char body ...) ...)
     (let ((c (bytevector-u8-ref obj 0)))
       (cond
         ((= (ascii char) c)
          body ...
          ) ...
         (else #f))))))

(define-syntax parse-header-string/clause
  (syntax-rules ()
    ((_ obj off ((string . param) head)) 
     (let* ((header-length (string-length string))
            (header-bytes (map (lambda (x) (ascii x)) (string->list string))))
       (and (<= header-length
                (- (bytevector-length obj) off))
            (for-all = (list-ec (: i header-length)
                                (bytevector-u8-ref obj (+ off i)))
                     header-bytes)
            (cons head '()) ;; FIXME: implement it.
            )))))

(define-syntax parse-header-string
  (syntax-rules ()
    ((_ obj off clause ...)
     (or (parse-header-string/clause obj off clause) ...))))

(define (parse-command/host bv)
  ;; Host commands
  ;;
  ;; g            (read-registers)
  ;; G            (write-registers bv)
  ;; m            (read-memory address size)
  ;; M            (write-memory address data)
  ;; c            (continue)
  ;; C            (continue/signal sig)
  ;; D            (detach)
  ;; s            (step)
  ;; S            (step/signal sig)
  ;;              (detatch)
  ;; k            (kill)
  ;; ?            (signal?)
  ;; qC           (current-thread?)
  ;; qfThreadInfo (threadinfo/first)
  ;; qsThreadInfo (threadinfo/next)
  ;; T            (thread-alive? id)
  ;; H?thead-id   (for-thread ID read-registers)
  ;;              (for-thread ID write-registers)

  (define (parse-query)
    (or
      (parse-header-string
        bv 1
        (("C") 'current-thread?)
        (("fThreadInfo") 'threadinfo/first)
        (("sThreadInfo") 'threadinfo/next))
      `(unknown-query ,(utf8->string bv))))

  (or
    (dispatch-topchar
      bv
      (#\? '(signal?))
      (#\g '(read-registers))
      (#\G `(write-registers . ,(gdb-scan bv 1 '(MEMORY))))
      (#\m `(read-memory . ,(gdb-scan bv 1 '(VALUE COMMA VALUE))))
      (#\M `(write-memory 
              .  ,(gdb-scan bv 1 '(VALUE COMMA VALUE COLON MEMORY))))
      (#\c '(continue))
      (#\C `(continue/signal ,(bytevector-u8-ref (car (gdb-scan bv 1 '(MEMORY))) 0)))
      (#\D '(detach))
      (#\s '(step))
      (#\S `(step/signal . ,(bytevector-u8-ref (car (gdb-scan bv 1 '(MEMORY))) 0)))
      (#\H `(for-thread ,(car (gdb-scan bv 2 '(VALUE))) 
                        ,(case (integer->char (bytevector-u8-ref bv 1))
                           ((#\g) 'read-registers)
                           ((#\G) 'write-registers)
                           (else 'UNKNOWN))))
      (#\k '(kill))
      (#\T `(thread-alive? . ,(gdb-scan bv 1 '(VALUE))))
      (#\q (parse-query)))
    '(unknown)))

(define (make-gdb-talker/host callback/send callback/event) ;; => ^[bv] ^[cmd cb]
  ; Target events
  ; S XX (signal n)
  ; W XX (exit n)
  ; X XX (terminate/signal n)
  ; ????? (unknown obj)
  ;
  ; Host commands
  ;;  +           (ACK)   special.
  ;;  -           (NACK)  special.
  ;; g            (read-registers) => bv
  ;; G            (write-registers bv)
  ;; m            (read-memory address size) => bv
  ;; M            (write-memory address data) => bv
  ;; c            (continue)
  ;; C            (continue/signal sig)
  ;; D            (detach)
  ;; H?thread-id  (for-thread ID read-registers)
  ;;              (for-thread ID write-registers)
  ;; qfThreadInfo (threadinfo/first)
  ;; qsThreadInfo (threadinfo/next)

  ; LATER
  ;; s            (step)
  ;; S            (step/signal sig)
  ;; (detatch)
  ;; k            (kill)
  ;; ?            (signal?)
  ;; qC           (current-thread?)
  ;; T            (thread-alive? id)

  ;; callback/send = (^[bv] ...)
  ;; callback/event = (^[evt] ...)
  ;; cb = (^[ok? obj] ...)
  (define state #f) ;; := WAIT-ACK | PACKET | WAIT-FOR-TRAP
  (define chunker #f)
  (define wait-for-trap? #f)
  (define (ok-packet? bv)
    (string=? "OK" (utf8->string bv)))
  (define (error-packet-value bv)
    (utf8->string bv))

  (define (memory-packet-value bv)
    ;; FIXME: Handle "xxxx" for don't care
    (define len (bytevector-length bv))
    (define bytecount (/ len 2))
    (define (b x) (or (nibble-num (bytevector-u8-ref bv x))
                      0))
    (define (read-nibble off)
      (+ (* 16 (b off))
         (b (+ 1 off))))
    (define (acc l)
      (fold-left (lambda (cur x)
                   (+ (* cur 256) x))
                 0
                 l))
    (unless (integer? bytecount)
      (assertion-violation
        'memory-packet-value
        "Short read"
        bv))
    ;(pp (list 'memory: (utf8->string bv)))
    (u8-list->bytevector (list-ec (: i bytecount)
                                  (read-nibble (* 2 i)))))

  (define (error-packet? bv)
    (= (bytevector-length bv) 3))

  (define (null-command ok? obj)
    ;; Ignore
    'ok)

  (define (encode obj)
    (cond
      ((integer? obj) ;; => addr encode (BE)
       (format-value obj))
      ((bytevector? obj) ;; (LE)
       (format-memory obj))
      ((string? obj) ;; => utf8
       (string->utf8 obj))
      ((char? obj)
       (bv-byte (char->integer obj)))
      (else 
        (assertion-violation 'gdb-encode
                             "Invalid gdb object"
                             obj))))

  (define current-command null-command) ;; = lambda
  (define (make-waiter cb fk)
    (lambda (ok? obj)
      (if ok? (cb obj) (fk #f obj))))
  (define (wait-for-trap cb)
    (make-waiter
      (lambda (obj)
        (cb #t obj))
      cb))
  (define (wait-for-generic cb)
    (make-waiter
      (lambda (obj)
        (if (ok-packet? obj)
          (cb #t #t)
          (cb #f (error-packet-value obj))))
      cb))
  (define (wait-for-memory cb)
    (make-waiter
      (lambda (obj)
        (if (error-packet? obj)
          (cb #f (error-packet-value obj))
          (cb #t (memory-packet-value obj))))
      cb))
  (define (trap-event obj)
    (cond
      ((and (bytevector? obj) (<= 3 (bytevector-length obj)))
       (let ((reason (integer->char (bytevector-u8-ref obj 0)))
             ;; FIXME: too early? We may have more data.
             (code (+ (* 16 (nibble-num (bytevector-u8-ref obj 1)))
                      (nibble-num (bytevector-u8-ref obj 2)))))
         (case reason
           ((#\S #\T)
            ;; T is extended trap event. 
            ;; FIXME: We will ignore any extended argument here
            (callback/event `(signal ,code)))
           ((#\W)
            (callback/event `(exit ,code)))
           ((#\X)
            (callback/event `(terminate/signal ,code)))
           (else
             (callback/event `(unknown ,obj))))))
      (else
       (callback/event '(unknown ,obj)))))

  (define (request trap-next? cb . bv)
    (define (do-encode e)
      (if (pair? e)
        (car e) ;; quote
        (encode e)))
    (define packet (escape/checksum (apply bv-concat (map do-encode bv))))
    (set! wait-for-trap? trap-next?)
    (set! current-command
      (lambda (ok? obj)
        ;; Retransmit on NACK
        (if (and (not ok?) (eq? obj 'NACK))
          (callback/send packet)
          (cb ok? obj))))
    (set! state 'WAIT-ACK)
    (callback/send packet))

  ;; Use list as quote OP
  (define bCOMMA (list (bv-byte COMMA)))
  (define bCOLON (list (bv-byte COLON)))
  (define bSEMICOLON (list (bv-byte SEMICOLON)))
  (define (push-command l cb)
    (match l
           (('ACK) (send-ACK) (cb #t))
           (('NACK) (send-NACK) (cb #t))
           (('read-memory address size) ;; m addr,length => memory
            (request #f
                     (wait-for-memory cb)
                     #\m
                     address
                     bCOMMA
                     size))

           (('write-memory address data) ;; M addr,length:MEM => generic
            (request #f
                     (wait-for-generic cb)
                     #\M
                     address
                     bCOMMA
                     (bytevector-length data)
                     bCOLON
                     data))

           (('read-registers) ;; g
            (request #f
                     (wait-for-memory cb)
                     #\g))
           (('write-registers data) ;; G MEM
            (request #f
                     (wait-for-generic cb)
                     #\G
                     data))
           (('continue) ;; c
            (request #t
                     (wait-for-trap cb)
                     #\c
                     ))
           (('continue addr) ;; c addr
            (request #t
                     (wait-for-trap cb)
                     #\c
                     addr))
           (('continue/signal sig)
            (request #t
                     (wait-for-trap cb)
                     #\C
                     sig))
           (('continue/signal sig addr)
            (request #t
                     (wait-for-trap cb)
                     #\C
                     sig
                     bSEMICOLON
                     addr))
           (('detach)
            (request #f
                     (wait-for-generic cb)
                     #\D))
           (('for-thread id 'read-registers)
            (request #f
                     (wait-for-memory cb)
                     #\H
                     id
                     #\g))
           (('for-thread id 'write-registers data)
            (request #f
                     (wait-for-generic cb)
                     #\H
                     id
                     #\G
                     data))
           (else
             (cb #f 'UNKNOWN)))
    #t)
  (define (send-ACK) (callback/send ACK-packet))
  (define (send-NACK) (callback/send NACK-packet))

  (define (procpacket obj)
    ;; obj := #f | #t | bv
    ;(pp (list 'procpacket: obj))
    (cond
      ((eq? obj #t) ;; Break??
       (set! state #f) ;; Invalid session
       (current-command #f 'INVALID)) 
      (obj
        (set! state #f)
        (send-ACK)
        (cond
          (wait-for-trap?
            (trap-event obj))
          (else
            (current-command #t obj))))
      (else
        (send-NACK))))

  (define (procbyte i)
    ;; FIXME: How to process stop-reply packets?
    ;(pp (list 'procbyte: (integer->char i) state))
    (case state
      ((#f) ;; Ignore a byte
       'ok)
      ((WAIT-ACK)
       (cond
         ((= i ACK)
          (when wait-for-trap?
            (current-command #t 'CONTINUE))
          (set! state 'PACKET))
         ((= i NACK)
          (when current-command
            ;; Try to retransmit
            (current-command #f 'NACK)))
         (else ;; Ignore
           'ok)))
      ((PACKET) 
       (chunker i))))

  (define (push-buffer bv)
    (do-ec (: i (bytevector-length bv))
           (procbyte (bytevector-u8-ref bv i))))


  (set! chunker (make-gdb-packet-chunker #t procpacket))
  (values push-buffer push-command))

(define (make-gdb-talker/target callback/send callback/event) ;; => ^[bv]
  ;; callback/send = (^[bv] ...)
  ;; callback/event = (^[evt] ...)
  (define (make-packet-parser callback/event) ;; => (^[bv/bool] ...)
    (lambda (buffer)
      (case buffer
        ((#f) ;; Checksum error
         (callback/send NACK-packet))
        ((#t) ;; Break
         (callback/event '(break)))
        (else
          (callback/send ACK-packet)
          (callback/event (parse-command/host buffer))))))

  (define chunker (make-gdb-packet-chunker #f (make-packet-parser callback/event)))
  (lambda (buffer)
    (do-ec (: i (bytevector-length buffer))
           (chunker (bytevector-u8-ref buffer i)))))

)
