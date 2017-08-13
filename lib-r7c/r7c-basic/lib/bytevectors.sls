(library (r7c-basic lib bytevectors)
         (export
           bytevector
           bytevector-copy
           bytevector-copy!
           bytevector-append
           utf8->string
           string->utf8
           make-bytevector)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c core error)
                 (r7c syntax if)
                 (r7c syntax and)
                 (r7c syntax or)
                 (r7c syntax let)
                 (r7c syntax cond)
                 (r7c heap core)
                 (r7c heap bytevector)
                 (r7c heap fixnum)
                 (r7c heap pair))

         
(define (make-bytevector len . fill?)
  (if (null? fill?)
    ($make-bytevector len)
    (let ((bv ($make-bytevector len))
          (fill (car fill?)))
      ($bytevector-fill! bv fill 0 len)
      bv)))

(define (string->utf8/itr! str bv cur start end)
  ;; FIXME: Not quite utf8
  (unless ($fx= start end)
    (let ((c0 (char->integer (string-ref str start))))
     (when ($fx> c0 #x80)
       (error "Invalid charactor" c0))
     (bytevector-u8-set! bv cur c0)
     (string->utf8/itr! str bv ($fx+ cur 1) ($fx+ start 1) end))))

(define (string->utf8/totallen str cur start end)
  (if ($fx= start end)
    cur
    (let ((c0 (char->integer (string-ref str start))))
      (cond
        (($fx< c0 #x80)
         (string->utf8/totallen str ($fx+ cur 1) ($fx+ start 1) end))
        (($fx< c0 #x800)
         (string->utf8/totallen str ($fx+ cur 2) ($fx+ start 1) end))
        (($fx< c0 #x10000)
         (string->utf8/totallen str ($fx+ cur 3) ($fx+ start 1) end))
        (($fx< c0 #x110000)
         (string->utf8/totallen str ($fx+ cur 4) ($fx+ start 1) end))
        (else
          (error "Invalid character" c0 start))))))

(define (string->utf8/pick str start end)
  (let* ((len (string->utf8/totallen str 0 start end))
         (bv ($make-bytevector len)))
    (string->utf8/itr! str bv 0 start end)
    bv))

(define (string->utf8 str . args)
  (if (null? args)
    (string->utf8/pick str 0 (string-length str))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (string->utf8/pick str start (string-length str))
        (string->utf8/pick str start (car d)))) ))

(define (utf8-char/4 c0 c1 c2 c3)
  (let ((b0 ($fx- c0 #xe0))
        (b1 ($fx- c1 #x80))
        (b2 ($fx- c2 #x80))
        (b3 ($fx- c3 #x80)))
    (integer->char
      ($fx+ b3 ($fx* ($fx+ b2 ($fx* ($fx+ b1 ($fx* b0 #x40)) #x40)) #x40)))))

(define (utf8-char/3 c0 c1 c2)
  (let ((b0 ($fx- c0 #xe0))
        (b1 ($fx- c1 #x80))
        (b2 ($fx- c2 #x80)))
    (integer->char
      ($fx+ b2 ($fx* ($fx+ b1 ($fx* b0 #x40)) #x40)))))

(define (utf8-char/2 c0 c1)
  (let ((b0 ($fx- c0 #xc0))
        (b1 ($fx- c1 #x80)))
    (integer->char
      ($fx+ b1 ($fx* b0 #x40)))))

(define (utf8->string/itr! bv str pos start end)
  (unless ($fx= start end)
    (let ((c0 (bytevector-u8-ref bv start)))
     (cond
       (($fx< c0 #x80)
        (string-set! str pos (integer->char c0))
        (utf8->string/itr! bv str ($fx+ pos 1) ($fx+ start 1) end))
       (($fx< c0 #xe0)
        (string-set! str pos
                     (utf8-char/2 c0
                                  (bytevector-u8-ref bv ($fx+ start 1))))
        (utf8->string/itr! bv str ($fx+ pos 1) ($fx+ start 2) end))
       (($fx< c0 #xf0)
        (string-set! str pos
                     (utf8-char/3 c0
                                  (bytevector-u8-ref bv ($fx+ start 1))
                                  (bytevector-u8-ref bv ($fx+ start 2))))
        (utf8->string/itr! bv str ($fx+ pos 1) ($fx+ start 3) end))
       (($fx< c0 #xf5)
        (string-set! str pos
                     (utf8-char/4 c0
                                  (bytevector-u8-ref bv ($fx+ start 1))
                                  (bytevector-u8-ref bv ($fx+ start 2))
                                  (bytevector-u8-ref bv ($fx+ start 3))))
        (utf8->string/itr! bv str ($fx+ pos 1) ($fx+ start 4) end))
       (else
         (error "Invalid character" c0 start))))))

(define (utf8->string/totallen bv cur start end)
  (if ($fx= start end)
    cur
    (let ((c0 (bytevector-u8-ref bv start)))
     (cond
       (($fx< c0 #x80)
        (utf8->string/totallen bv ($fx+ cur 1) 
                               ($fx+ start 1) end))
       (($fx< c0 #xe0)
        (utf8->string/totallen bv ($fx+ cur 1)
                               ($fx+ start 2) end))
       (($fx< c0 #xf0)
        (utf8->string/totallen bv ($fx+ cur 1)
                               ($fx+ start 3) end))
       (($fx< c0 #xf5)
        (utf8->string/totallen bv ($fx+ cur 1)
                               ($fx+ start 4) end))
       (else
         (error "Invalid character" c0 start))))))

(define (utf8->string/pick bv start end)
  (let* ((len (utf8->string/totallen bv 0 start end))
         (str ($make-string len)))
    (utf8->string/itr! bv str 0 start end)
    str))

(define (utf8->string bv . args)
  (if (null? args)
    (utf8->string/pick bv 0 (bytevector-length bv))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (utf8->string/pick bv start (bytevector-length bv))
        (utf8->string/pick bv start (car d))))))         
         
(define (bytevector-append/itr! bv pos queue)
  (unless (null? queue)
    (let* ((a (car queue))
           (len (bytevector-length a)))
      ($bytevector-copy! bv pos a 0 len)
      (bytevector-append/itr! bv ($fx+ pos len) (cdr queue)))))

(define (bytevector-append/totallen cur queue)
  (if (null? queue)
    cur
    (bytevector-append/totallen ($fx+ cur (bytevector-length (car queue)))
                                (cdr queue))))
(define (bytevector-append . queue)
  (let ((bv ($make-bytevector (bytevector-append/totallen 0 queue))))
   (bytevector-append/itr! bv 0 queue)
   bv))
         
#|
(define (bytevector-copy!/itr+ to at from start end)
  (unless ($fx= start end)
    (bytevector-u8-set! to at (bytevector-u8-ref from start))
    (bytevector-copy!/itr+ to ($fx+ at 1) from ($fx+ start 1) end)))

(define (bytevector-copy!/itr- to to-end from start end)
  (unless ($fx= start end)
    (bytevector-u8-set! to to-end (bytevector-u8-ref from end))
    (bytevector-copy!/itr- to ($fx- to-end 1) from start ($fx- end 1))))

(define (bytevector-copy!/pick to at from start end)
  (unless ($fx= start end)
    (let ((to-end ($fx+ at ($fx- end start))))
     (if ($fx< start to-end)
       (bytevector-copy!/itr- to ($fx- to-end 1) from 
                              ($fx- start 1) ($fx- end 1))
       (bytevector-copy!/itr+ to at from start end)))))
         
(define (bytevector-copy! to at from . args)
  (if (null? args)
    (bytevector-copy!/pick to at from 0 (bytevector-length from))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (bytevector-copy!/pick to at from start (bytevector-length from))
        (bytevector-copy!/pick to at from start (car d))))))

(define (bytevector-copy/itr! bv dest pos start end)
  (unless ($fx= start end)
    (bytevector-u8-set! dest pos (bytevector-u8-ref bv start))
    (bytevector-copy/itr! bv dest ($fx+ pos 1) ($fx+ start 1) end)))

(define (bytevector-copy/pick bv start end)
  (let ((dest ($make-bytevector ($fx- end start))))
   (bytevector-copy/itr! bv dest 0 start end)
   dest))

(define (bytevector-copy bv . args)
  (if (null? args)
    (bytevector-copy/pick bv 0 (bytevector-length bv))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (bytevector-copy/pick bv start (bytevector-length bv))
        (bytevector-copy/pick bv start (car d))))))
|#

(define (bytevector-copy! to at from . args)
  (if (null? args)
    ($bytevector-copy! to at from 0 (bytevector-length from))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        ($bytevector-copy! to at from start (bytevector-length from))
        ($bytevector-copy! to at from start (car d))))))

(define (bytevector-copy/pick bv start end)
  (let ((dest ($make-bytevector ($fx- end start))))
   ($bytevector-copy! dest 0 bv start end)
   dest))

(define (bytevector-copy bv . args)
  (if (null? args)
    (bytevector-copy/pick bv 0 (bytevector-length bv))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (bytevector-copy/pick bv start (bytevector-length bv))
        (bytevector-copy/pick bv start (car d))))))
         
(define (bytevector/itr! bv pos u8*)
  (unless (null? u8*)
    (bytevector-u8-set! bv pos (car u8*))
    (bytevector/itr! bv ($fx+ pos 1) (cdr u8*))))

(define (bytevector . u8)
  (let* ((len (length u8))
         (bv ($make-bytevector len)))
    (bytevector/itr! bv 0 u8)
    bv))
)
