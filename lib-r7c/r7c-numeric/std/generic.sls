(library (r7c-numeric std generic)
         (export = < > <= >=
                 + * - /
                 number? complex? real? integer?
                 exact? inexact? exact-integer?
                 quotient remainder modulo
                 floor ceiling truncate round
                 expt
                 inexact exact)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap flonum)
                 (r7c syntax cond)
                 (r7c syntax and)
                 (r7c syntax or)
                 (r7c syntax let)
                 (r7c-numeric std division))

(define (=/itr b queue)
  (if (null? queue) 
    #t
    (and (= b (car queue))
         (=/itr b (cdr queue)))))

(define (= a b . queue)
  (if (null? queue)
    (if ($flonum? a)
      (if ($flonum? b)
        ($fl= a b)
        ($fl= a ($fx->fl b)))
      (if ($flonum? b)
        ($fl= ($fx->fl a) b)
        ($fx= a b)))
    (and (= a b)
         (=/itr b queue))))


(define (</itr b queue)
  (if (null? queue) 
    #t
    (let ((a (car queue)))
     (and (< b a)
          (</itr a (cdr queue))))))

(define (< a b . queue)
  (if (null? queue)
    (if ($flonum? a)
      (if ($flonum? b)
        ($fl< a b)
        ($fl< a ($fx->fl b)))
      (if ($flonum? b)
        ($fl< ($fx->fl a) b)
        ($fx< a b)))
    (and (< a b)
         (</itr b queue))))

(define (>/itr b queue)
  (if (null? queue) 
    #t
    (let ((a (car queue)))
     (and (> b a)
          (>/itr a (cdr queue))))))

(define (> a b . queue)
  (if (null? queue)
    (if ($flonum? a)
      (if ($flonum? b)
        ($fl> a b)
        ($fl> a ($fx->fl b)))
      (if ($flonum? b)
        ($fl> ($fx->fl a) b)
        ($fx> a b)))
    (and (> a b)
         (>/itr b queue))))

(define (<=/itr b queue)
  (if (null? queue) 
    #t
    (let ((a (car queue)))
     (and (<= b a)
          (<=/itr a (cdr queue))))))

(define (<= a b . queue)
  (if (null? queue)
    (if ($flonum? a)
      (if ($flonum? b)
        ($fl<= a b)
        ($fl<= a ($fx->fl b)))
      (if ($flonum? b)
        ($fl<= ($fx->fl a) b)
        ($fx<= a b)))
    (and (<= a b)
         (<=/itr b queue))))

(define (>=/itr b queue)
  (if (null? queue) 
    #t
    (let ((a (car queue)))
     (and (>= b a)
          (>=/itr a (cdr queue))))))

(define (>= a b . queue)
  (if (null? queue)
    (if ($flonum? a)
      (if ($flonum? b)
        ($fl>= a b)
        ($fl>= a ($fx->fl b)))
      (if ($flonum? b)
        ($fl>= ($fx->fl a) b)
        ($fx>= a b)))
    (and (>= a b)
         (>=/itr b queue))))

(define (%plus/itr cur-flonum? cur queue)
  (if (null? queue)
    cur
    (let ((a (car queue)))
     (if cur-flonum?
       (if ($flonum? a)
         (%plus/itr #t ($fl+ cur a) (cdr queue))
         (%plus/itr #t ($fl+ cur ($fx->fl a)) (cdr queue)))
       (if ($flonum? a)
         (%plus/itr #t ($fl+ ($fx->fl cur) a) (cdr queue))
         (%plus/itr #f ($fx+ cur a) (cdr queue)))))))

(define (+ . queue)
  (%plus/itr #f 0 queue))

(define (%mul/itr cur-flonum? cur queue)
  (if (null? queue)
    cur
    (let ((a (car queue)))
     (if cur-flonum?
       (if ($flonum? a)
         (%mul/itr #t ($fl* cur a) (cdr queue))
         (%mul/itr #t ($fl* cur ($fx->fl a)) (cdr queue)))
       (if ($flonum? a)
         (%mul/itr #t ($fl* ($fx->fl cur) a) (cdr queue))
         (%mul/itr #f ($fx* cur a) (cdr queue)))))))

(define (* . queue)
  (%mul/itr #f 1 queue))

(define (%sub/itr acc-flonum? acc queue)
  (if (null? queue)
    acc
    (let ((a (car queue)))
     (if acc-flonum?
       (if ($flonum? a)
         (%sub/itr #t ($fl- acc a) (cdr queue))
         (%sub/itr #t ($fl- acc ($fx->fl a)) (cdr queue)))
       (if ($flonum? a)
         (%sub/itr #t ($fl- ($fx->fl acc) a) (cdr queue))
         (%sub/itr #f ($fx- acc a) (cdr queue)))))))

(define (- x . queue)
  (if (null? queue)
    (if ($flonum? x)
      ($fl- 0.0 x)
      ($fx- 0 x))
    (if ($flonum? x)
      (%sub/itr #t x queue)
      (%sub/itr #f x queue))))

(define (%div/itr acc queue)
  ;; FIXME: Do not always promote to flonum...
  (if (null? queue)
    acc
    (let ((a (car queue)))
     (if ($flonum? a)
       (%div/itr ($fl/ acc a) (cdr queue))
       (%div/itr ($fl/ acc ($fx->fl a)) (cdr queue))))))

(define (/ x . queue)
  (if (null? queue)
    (if ($flonum? x)
      ($fl/ 1.0 x)
      (if ($fx= 1 x) 1 ($fl/ 1.0 ($fx->fl x))))
    (%div/itr x queue)))

(define (number? x)
  (or ($fixnum? x) ($flonum? x)))

(define (complex? x) #f)
(define (real? x) (number? #t))
(define (integer? x) ($fixnum? x))
(define (exact? x) ($fixnum? x))
(define (inexact? x) ($flonum? x))
(define (exact-integer? x) ($fixnum? x))
(define quotient truncate-quotient)
(define remainder truncate-remainder)
(define modulo floor-remainder)
(define (floor x)
  (cond
    (($fixnum? x) x)
    (else ($fl-floor x))))
(define (ceiling x)
  (cond
    (($fixnum? x) x)
    (else ($fl-ceiling x))))
(define (truncate x)
  (cond
    (($fixnum? x) x)
    (else ($fl-truncate x))))
(define (round x)
  (cond
    (($fixnum? x) x)
    (else ($fl-round x))))
(define (expt a b)
  (if ($flonum? a)
    (if ($flonum? b)
      ($fl-expt a b)
      ($fl-expt a ($fx->fl b)))
    (if ($flonum? b)
      ($fl-expt ($fx->fl a) b)
      ($fx-expt a b))))
(define (inexact a) ($fx->fl a))
(define (exact a) ($fl->fx a))

)
