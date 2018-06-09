(define (vector-map4/itr! v pos len proc a b c d)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)
                       (vector-ref c pos)
                       (vector-ref d pos)))
    (vector-map4/itr! v (+ pos 1) len proc a b c d)))

(define (vector-map4 proc a b c d)
  (let* ((len (min (vector-length a) (vector-length b)
                   (vector-length c) (vector-length d)))
         (v (make-vector len)))
    (vector-map4/itr! v 0 len proc a b c d)
    v))

(define (vector-map3/itr! v pos len proc a b c)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)
                       (vector-ref c pos)))
    (vector-map3/itr! v (+ pos 1) len proc a b c)))

(define (vector-map3 proc a b c)
  (let* ((len (min (vector-length a) (vector-length b) (vector-length c)))
         (v (make-vector len)))
    (vector-map3/itr! v 0 len proc a b c)
    v))

(define (vector-map2/itr! v pos len proc a b)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)
                       (vector-ref b pos)))
    (vector-map2/itr! v (+ pos 1) len proc a b)))

(define (vector-map2 proc a b)
  (let* ((len (min (vector-length a) (vector-length b)))
         (v (make-vector len)))
    (vector-map2/itr! v 0 len proc a b)
    v))

(define (vector-map1/itr! v pos len proc a)
  (unless (= pos len)
    (vector-set! v pos
                 (proc (vector-ref a pos)))
    (vector-map1/itr! v (+ pos 1) len proc a)))

(define (vector-map1 proc a)
  (let* ((len (vector-length a))
         (v (make-vector len)))
    (vector-map1/itr! v 0 len proc a)
    v))

(define (vector-map proc a . args)
  (if (null? args)
    (vector-map1 proc a)
    (let ((b (car args))
          (bb (cdr args)))
      (if (null? bb)
        (vector-map2 proc a b)
        (let ((c (car bb))
              (cc (cdr bb)))
          (if (null? cc)
            (vector-map3 proc a b c)
            (let ((d (car cc))
                  (dd (cdr cc)))
              (if (null? dd)
                (vector-map4 proc a b c d)
                (error "Too many...")))))))))