(library (r7c-basic lib vectors)
         (export vector
                 vector->list
                 list->vector
                 vector->string
                 string->vector
                 vector-copy
                 vector-copy!
                 vector-append
                 vector-fill!
                 make-vector)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c core error)
                 (r7c syntax if)
                 (r7c syntax and)
                 (r7c syntax or)
                 (r7c syntax let)
                 (r7c syntax cond)
                 (r7c heap core)
                 (r7c heap vector)
                 (r7c heap fixnum)
                 (r7c heap pair))

(define (make-vector len . fill?)
  (if (null? fill?)
    ($make-vector len)
    (let ((v ($make-vector len)))
     ($vector-fill! v (car fill?) 0 len)
     v)))

#|
(define (vector-fill!/itr vec fill start end)
  (unless ($fx= start end)
    (vector-set! vec start fill)
    (vector-fill!/itr vec fill ($fx+ start 1) end))) 
|#

(define (vector-fill! vec fill . args)
  (if (null? args)
    ($vector-fill! vec fill 0 (vector-length vec))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        ($vector-fill! vec fill start (vector-length vec))
        ($vector-fill! vec fill start (car d))))))

(define (vector-append/itr! vec pos queue)
  (unless (null? queue)
    (let* ((a (car queue))
           (len (vector-length a)))
      ($vector-copy! vec pos a 0 len)
      (vector-append/itr! vec ($fx+ pos len) (cdr queue)))))

(define (vector-append/totallen cur queue)
  (if (null? queue)
    cur
    (vector-append/totallen ($fx+ cur (vector-length (car queue))) 
                            (cdr queue))))

(define (vector-append . queue)
  (let ((v ($make-vector (vector-append/totallen 0 queue))))
   (vector-append/itr! v 0 queue)
   v))

#|
(define (vector-copy!/itr+ to at from start end)
  (unless ($fx= start end)
    (vector-set! to at (vector-ref from end))
    (vector-copy!/itr+ to ($fx+ at 1) from ($fx+ start 1) end)))

(define (vector-copy!/itr- to to-end from start end)
  (unless ($fx= start end)
    (vector-set! to to-end (vector-ref from end))
    (vector-copy!/itr- to ($fx- to-end 1) from start ($fx- end 1))))
         
(define (vector-copy!/pick to at from start end)
  (unless ($fx= start end)
    ;; Decide direction
    (let ((to-end ($fx+ at ($fx- end start))))
     (if ($fx< start to-end)
       (vector-copy!/itr- to ($fx- to-end 1) from ($fx- start 1) ($fx- end 1))
       (vector-copy!/itr+ to at from start end)))))

(define (vector-copy! to at from . args)
  (if (null? args)
    (vector-copy!/pick to at from 0 (vector-length from))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (vector-copy!/pick to at from start (vector-length from))
        (vector-copy!/pick to at from start (car d))))))

(define (vector-copy/itr! vec v pos start end)
  (unless ($fx= start end)
    (vector-set! v pos (vector-ref vec start))
    (vector-copy/itr! vec v ($fx+ pos 1) ($fx+ start 1) end)))
|#
         
(define (vector-copy! to at from . args)
  (if (null? args)
    ($vector-copy! to at from 0 (vector-length from))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        ($vector-copy! to at from start (vector-length from))
        ($vector-copy! to at from start (car d))))))

(define (vector-copy/pick vec start end)
  (let ((v ($make-vector ($fx- end start))))
   ($vector-copy! v 0 vec start end)
   v))
         
(define (vector-copy vec . args)
  (if (null? args)
    (vector-copy/pick vec 0 (vector-length vec))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (vector-copy/pick vec start (vector-length vec))
        (vector-copy/pick vec start (car d))))))

(define (vector . e) (list->vector e))

(define (vector->list/itr! vec cur start end)
  (unless ($fx= start end)
    (let ((c (cons (vector-ref vec start) '())))
     (set-cdr! cur c)
     (vector->list/itr! vec c ($fx+ start 1) end))))

(define (vector->list/itr vec start end)
  (cond
    (($fx= start end) '())
    (else
      (let ((c (cons (vector-ref vec start) '())))
       (vector->list/itr! vec c ($fx+ start 1) end)
       c))))

(define (vector->list vec . args)
  (if (null? args)
     (vector->list/itr vec 0 (vector-length vec)) 
     (let ((start (car args))
           (d (cdr args)))

       (if (null? d)
         (vector->list/itr vec start (vector-length vec))
         (vector->list/itr vec start (car d))))))

(define (list->vector/itr! vec pos l)
  (unless (null? l)
    (vector-set! vec pos (car l))
    (list->vector/itr! vec ($fx+ pos 1) (cdr l))))

(define (list->vector l)
  (let ((v (make-vector ($fx-length l))))
   (list->vector/itr! v 0 l)
   v))

(define (vector->string/itr! vec str pos start end)
  (unless ($fx= start end)
    (string-set! str pos (vector-ref vec start))
    (vector->string/itr! vec str ($fx+ pos 1) ($fx+ start 1) end)))

(define (vector->string/pick vec start end)
  (let ((s ($make-string ($fx- end start))))
   (vector->string/itr! vec s 0 start end)
   s))

(define (vector->string vec . args)
  (if (null? args)
    (vector->string/pick vec 0 (vector-length vec))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (vector->string/pick vec start (vector-length vec))
        (vector->string/pick vec start (car d))))))

(define (string->vector/itr! str v pos start end)
  (unless ($fx= start end)
    (vector-set! v pos (string-ref str start))
    (string->vector/itr! str v ($fx+ pos 1) ($fx+ start 1) end)))

(define (string->vector/pick str start end)
  (let ((v ($make-vector ($fx- end start))))
   (string->vector/itr! str v 0 start end)
   v))

(define (string->vector str . args)
  (if (null? args)
    (string->vector/pick str 0 (string-length str))
    (let ((start (car args))
          (d (cdr args)))
      (if (null? d)
        (string->vector/pick str start (string-length str))
        (string->vector/pick str start (car d))))))

)
