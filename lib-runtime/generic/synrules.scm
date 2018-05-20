;; Generic syntax-rules implementation for define-macro platforms

;; Runtime for chibi-scheme's synrule implementation
;; yuni/gensym and yuni/identifier? are implementation specific
(define (yuni/identifier->symbol x) x)
(define (yuni/any1 pred l)
  (and (pair? l)
       (pred (car l))
       (yuni/any1 pred (cdr l))))
(define (yuni/find1 pred l)
  (if (pair? l)
    (let ((a (car l)))
     (if (pred a)
       a
       (yuni/find1 pred (cdr l))))
    #f))
(define (yuni/cons-source a b c) (cons a b))
(define (yuni/length*-itr n obj)
  (if (pair? obj)
    (yuni/length*-itr (+ n 1) (cdr obj))
    n))
(define (yuni/length* obj)
  (yuni/length*-itr 0 obj))

;; Other synrule libraries
(define (yuni/reserved-underscore-index sym)
  (case sym
    ((__0) 0)
    ((__1) 1)
    ((__2) 2)
    ((__3) 3)
    ((__4) 4)
    ((__5) 5)
    ((__6) 6)
    ((__7) 7)
    ((__8) 8)
    ((__9) 9)
    (else #f)))

(define (yuni/make-synrule-baselib) 
  (define cache #f)
  (define cache-index #f)
  (define (do-gensym idx)
    (yuni/gensym 'bogus))
  (define (fillcache! idx)
    (let ((s (do-gensym idx)))
     (vector-set! cache idx s)
     s))
  (define (take idx)
    (cond
      (cache-index
        (if (= cache-index idx) 
          cache
          (begin
            (let ((c cache))
             (set! cache (make-vector 10 #f))
             (vector-set! cache cache-index c)
             (set! cache-index #f))
            (fillcache! idx))))
      ((vector? cache)
       (or (vector-ref cache idx)
           (fillcache! idx)))
      (else
        (set! cache-index idx)
        (set! cache (do-gensym idx)))))
  (lambda (sym)
    (let ((i (yuni/reserved-underscore-index sym)))
     (or (and i (take i))
         sym))))

(define (yuni/synrule-compare x y) (eq? x y))

(define-macro (define-syntax name synrule)
  (let ((tran 
          (cadr 
            (yuni/syntax-rules-transformer 
              synrule 
              yuni/gensym (yuni/make-synrule-baselib) yuni/synrule-compare)))
        (args (yuni/gensym 'args))
        (a (yuni/gensym 'output)))
    `(define-macro (,name . ,args)
       (let ((,a (,tran (cons ',name ,args)
                        yuni/gensym (yuni/make-synrule-baselib) yuni/synrule-compare)))
         ;(display (list 'OUT: a))
         ;(newline)
         ,a))))

