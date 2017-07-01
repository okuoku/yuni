(library (r7c-numeric std misc)
         (export
           zero? positive? negative? odd? even?
           square max min abs
           ; gcd lcm
           )
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c heap fixnum)
                 (r7c heap flonum)
                 (r7c syntax cond)
                 (r7c syntax and)
                 (r7c syntax or)
                 (r7c syntax let)
                 (r7c-numeric std generic))
         
         
(define (zero? x) (= 0 x))         
(define (positive? x) (< 0 x))
(define (negative? x) (> 0 x))
(define (odd? x) (even? (- x 1)))
(define (even? x) (= 0 (modulo x 2)))
(define (square x) (* x x))

(define (max/itr x-flonum? x queue)
  (if (null? queue)
    x
    (let ((a (car queue)))
     (if x-flonum?
       (if ($flonum? a)
         (if ($fl< x a)
           (max/itr #t a (cdr queue))
           (max/itr #t x (cdr queue)))
         (let ((fa ($fx->fl a)))
          (if ($fl< x fa)
            (max/itr #t fa (cdr queue))
            (max/itr #t x (cdr queue)))))
       (if ($flonum? a)
         (let ((fx ($fx->fl x)))
          (if ($fl< fx a)
            (max/itr #t a (cdr queue))
            (max/itr #t fx (cdr queue))))
         (if ($fx< x a)
           (max/itr #f a (cdr queue))
           (max/itr #f x (cdr queue))))))))

(define (max x . queue) (max/itr ($flonum? x) x queue))
         
(define (min/itr x-flonum? x queue)
  (if (null? queue)
    x
    (let ((a (car queue)))
     (if x-flonum?
       (if ($flonum? a)
         (if ($fl> x a)
           (min/itr #t a (cdr queue))
           (min/itr #t x (cdr queue)))
         (let ((fa ($fx->fl a)))
          (if ($fl> x fa)
            (min/itr #t fa (cdr queue))
            (min/itr #t x (cdr queue)))))
       (if ($flonum? a)
         (let ((fx ($fx->fl x)))
          (if ($fl> fx a)
            (min/itr #t a (cdr queue))
            (min/itr #t fx (cdr queue))))
         (if ($fx> x a)
           (min/itr #f a (cdr queue))
           (min/itr #f x (cdr queue))))))))

(define (min x . queue) (min/itr ($flonum? x) x queue))

(define (abs x) (if (< x 0) (- x) x))
         
         )
