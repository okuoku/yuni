(library (r7c-basic lib strings)
         (export
           string
           substring
           string-append
           string->list
           list->string
           string-copy
           string-copy!
           string-fill!
           make-string
           string=?  string<?  string>?  string<=?  string>=?)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c core error)
                 (r7c syntax if)
                 (r7c syntax and)
                 (r7c syntax or)
                 (r7c syntax let)
                 (r7c syntax cond)
                 (r7c syntax unless)
                 (r7c heap listloop)
                 (r7c heap core)
                 (r7c heap fixnum)
                 (r7c heap pair))

;;

(define (string-compare/2itr comp nil-win ashort? a b start end)
  (let ((ac (char->integer (string-ref a start)))
        (bc (char->integer (string-ref b start))))
    (if ($fx= start end)
      (if ($fx= ac bc)
        (if ashort? nil-win (not nil-win))
        (comp ac bc))
      (if ($fx= ac bc)
        (string-compare/2itr comp nil-win ashort? a b
                             ($fx+ start 1) end)
        (comp ac bc)))))

(define (string-compare/2itreq comp a b start end)
  (let ((ac (char->integer (string-ref a start)))
        (bc (char->integer (string-ref b start))))
    (if ($fx= start end)
      (comp ac bc)
      (if ($fx= ac bc)
        (string-compare/2itreq comp a b ($fx+ start 1) end)
        (comp ac bc)))))

(define (string-compare/2 comp nil-win eq-win a b)
  (let ((alen (string-length a))
        (blen (string-length b)))
    (cond
      (($fx= alen blen)
       (if ($fx= alen 0)
         eq-win ;; both nil-string
         (string-compare/2itreq comp a b 0 ($fx- alen 1))))
      (($fx< alen blen)
       (if ($fx= alen 0)
         nil-win ;; A was a nil-string
         (string-compare/2itr comp nil-win #t a b 0 ($fx- alen 1))))
      (else
        (if ($fx= blen 0)
          (not nil-win) ;; B was a nil-string
          (string-compare/2itr comp nil-win #f a b 0 ($fx- blen 1)))))))

(define (string-compare/queue comp nil-win? eq-win? a b queue)
  (and (string-compare/2 comp nil-win? eq-win? a b)
       (or (null? queue)
           (string-compare/queue comp nil-win? eq-win? b 
                                 (car queue) (cdr queue)))))

(define (string>=? a b . queue)
  (string-compare/queue $fx>= #f #t a b queue))

(define (string<=? a b . queue)
  (string-compare/queue $fx<= #t #t a b queue))

(define (string>? a b . queue)
  (string-compare/queue $fx> #f #f a b queue))

(define (string<? a b . queue)
  (string-compare/queue $fx< #t #f a b queue))

(define (string=?/itr a end queue)
  (or (null? queue)
      (let ((b (car queue)))
       (and ($fx= end (string-length b))
            (string=?/compare a b 0 end)
            (string=?/itr a end (cdr queue))))))

(define (string=?/compare a b start end)
  (or ($fx= start end)
      (let ((ac (char->integer (string-ref a start)))
            (bc (char->integer (string-ref b start))))
        (and ($fx= ac bc)
             (string=?/compare a b ($fx+ start 1) end)))))

(define (string=? a b . queue)
  (let ((alen (string-length a))
        (blen (string-length b)))
    (and ($fx= alen blen)
         (string=?/compare a b 0 alen)
         (or (null? queue)
             (string=?/itr a alen queue)))))

(define (make-string len . fill?)
  (if (null? fill?)
    ($make-string len)
    (let ((fill (car fill?))
          (s ($make-string len)))
      ($string-fill! s fill 0 len)
      s)))

#|
;; Now $string-fill coreop
(define (string-fill!/itr str fill start end)
  (unless ($fx= start end)
    (string-set! str start fill)
    (string-fill!/itr str fill ($fx+ start 1) end)))
|#

(define (string-fill! str fill . args)
  (if (null? args)
    ($string-fill! str fill 0 (string-length str))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        ($string-fill! str fill start (string-length str))
        ($string-fill! str fill start (car d))))))

(define (string->list/itr+! str cur start end)
  (unless ($fx= start end)
    (let ((c (cons (string-ref str start) '())))
     (set-cdr! cur c)
     (string->list/itr+! str c ($fx+ start 1) end))))

(define (string->list/itr str start end)
  (cond
    (($fx= start end) '())
    (($fx< end start)
     (error "Invalid parameter" start end) )
    (else
      (let ((c (cons (string-ref str start) '())))
       (string->list/itr+! str c ($fx+ start 1) end)
       c))))

(define (string->list str . args)
  (if (null? args)
    (if ($fx= (string-length str) 0)
      '()
      (let ((c (cons (string-ref str 0) '())))
       (string->list/itr+! str c 1 (string-length str)) 
       c))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (string->list/itr str start (string-length str))
        (string->list/itr str start (car d))))))

(define (list->string/itr! s cur l)
  (unless (null? l)
    (string-set! s cur (car l))
    (list->string/itr! s ($fx+ cur 1) (cdr l))))

(define (list->string l)
  (let* ((len ($fx-length l))
         (s ($make-string len)))
    (list->string/itr! s 0 l)
    s))

(define (string-copy str . rest)
  (if (null? rest)
    (substring str 0 (string-length str))
    (let ((start (car rest))
          (d (cdr rest)))
      (if (null? d)
        (substring str start (string-length str))
        (substring str start (car d))))))

(define (string . c*) (list->string c*))
(define (substring str start end)
  (let ((s ($make-string ($fx- end start))))
   ($string-copy! s 0 str start end)
   s))

(define (string-append/paste s cur queue)
  (unless (null? queue)
    (let* ((x (car queue))
           (l (string-length x)))
     ($string-copy! s cur x 0 l)
     (string-append/paste s ($fx+ cur l) (cdr queue)))))

(define (string-append/calctotal cur queue q)
  (cond
    ((null? queue)
     (let ((s ($make-string cur)))
      (string-append/paste s 0 q)
      s))
    (else
      (string-append/calctotal ($fx+ cur (string-length (car queue)))
                               (cdr queue)
                               q))))

(define (string-append . queue)
  (cond
    ((null? queue) 
     ;; FIXME: Really?
     "")
    (else
      (string-append/calctotal 0 queue queue))))

#|
(define (string-copy! to at from . rest)
  (cond
    ((null? rest)
     (string-copy!/4 to at from 0 (string-length from)))
    (else
      (cond
        ((null? (cdr rest))
         (string-copy!/4 to at from (car rest) (string-length from)))
        (else
          (string-copy!/4 to at from (car rest) (cadr rest)))))))

(define (string-copy!/4+ to at from start end)
  (unless ($fx= start end)
    (string-set! to at (string-ref from start))
    (string-copy!/4+ to ($fx+ at 1) from ($fx+ start 1) end))) 

(define (string-copy!/4- to to-end from start end)
  (unless ($fx= start end)
    (string-set! to to-end (string-ref from end))
    (string-copy!/4- to ($fx- to-end 1) from start ($fx- end 1))))

(define (string-copy!/4 to at from start end)
  (cond
    (($fx= start end)
     ;; Do nothing
     #t)
    (($fx< end start)
     (error "Invalid param" start end))
    (else
      ;; Decide direction
      ;; If we would overwrite from[start], begin with start
      (let ((to-end ($fx+ at ($fx- end start))))
       (if ($fx< start to-end)
         (string-copy!/4- to ($fx- to-end 1) from ($fx- start 1) ($fx- end 1))
         (string-copy!/4+ to at from start end))))))
|#

(define (string-copy! to at from . rest)
  (cond
    ((null? rest)
     ($string-copy! to at from 0 (string-length from)))
    (else
      (cond
        ((null? (cdr rest))
         ($string-copy! to at from (car rest) (string-length from)))
        (else
          ($string-copy! to at from (car rest) (cadr rest)))))) )

         
)
