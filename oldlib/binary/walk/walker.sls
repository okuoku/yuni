(library (yuni binary walk walker)
         (export
           layout->const-ref
           layout->bv-ref 
           layout->bv-ref/bv
           layout->walker
           layout->walker/bv ;; FIXME: 
           )
         (import (rnrs)
                 (shorten)
                 (yuni core)
                 (yuni async)
                 (yuni binary walk common))

(define (layout->bv-ref/bv layout* typename)
  (layout->bv-ref/internal #f layout* typename))

(define (layout->bv-ref layout* typename)
  (layout->bv-ref/internal #t layout* typename))

(define (layout->bv-ref/internal translate-result? layout* typename)
  ;; FIXME: Merge with layout->walker
  (define (check l)
    (if (pair? l)
      (let ((a (car l))
            (d (cdr l)))
        (or (and (is-a? a type)
                 (eq? typename (~ a 'name))
                 a)
            (check d)))
      #f))
  (define mytype (check layout*))
  (define entry-ht (make-hashtable equal? equal-hash))
  (define (pickup bv addr size)
    (define ret (make-bytevector size))
    (bytevector-copy! bv 0 ret 0 size)
    ret)
  (define (ref bv l)
    (define (return to-string? value)
      (if translate-result?
        (if to-string?
          (bv->str value)
          (bv->int value))
        value))
    (define myname (if (list? l) l (list l)))
    (let ((p (hashtable-ref entry-ht myname #f)))
      (and p
           (return (caddr p)
                   (pickup bv (car p) (cadr p)))
           )))

  ;; Generate entry-ht
  (for-each (^e
              (when (and (is-a? e entry))
                (let-with e (parent name offset size type)
                  (when (or (symbol? parent) (list? parent))
                    (let ((myname (append (if (list? parent) (cdr parent) '())
                                          (list name))))
                      (hashtable-set! entry-ht myname (list offset size
                                                            (eq? 'string type))))))))
            layout*)
  ref)

(define (layout->walker layout* typename reader)
  (layout->walker/internal #t layout* typename reader))
(define (layout->walker/bv layout* typename reader)
  (layout->walker/internal #f layout* typename reader))

(define (layout->const-ref layout*)
  (define entry-ht (make-eq-hashtable))
  (for-each (^e
              (when (and (is-a? e entry) (~ e 'constant?))
                (let-with e (name value)
                  (hashtable-set! entry-ht name value))))
            layout*)
  (^[sym]
    (hashtable-ref entry-ht sym #f)))

(define (layout->walker/internal translate-result? layout* typename reader)
  (define (check l)
    (if (pair? l)
      (let ((a (car l))
            (d (cdr l)))
        (or (and (is-a? a type)
                 (eq? typename (~ a 'name))
                 a)
            (check d)))
      #f))
  (define mytype (check layout*))
  (define entry-ht (make-hashtable equal-hash equal?))
  (define (walk offset l cb)
    (define (return to-string? value)
      (if translate-result?
        (if to-string?
          (cb (bv->str value))
          (cb (bv->int value)))
        (cb value)))
    (define myname (if (list? l) l (list l)))
    (let ((p (hashtable-ref entry-ht myname #f)))
      (if p
        (seq (=> reader (+ offset (car p)) (cadr p) => r)
             (if r
               (return (caddr p) r)
               (cb #f)))
        (cb #f))))

  ;; Generate entry-ht
  (for-each (^e
              (when (and (is-a? e entry))
                (let-with e (parent name offset size type)
                  (when (or (symbol? parent) (list? parent))
                    (let ((myname (append (if (list? parent) (cdr parent) '())
                                          (list name))))
                      (hashtable-set! entry-ht myname (list offset size
                                                            (eq? 'string type))))))))
            layout*)
  #|
  (let-values (((key value) (hashtable-entries entry-ht)))
              (for-each (^[k v]
                          (write (list 'key: k 'value: v))(newline))
                        (vector->list key)
                        (vector->list value)))
  |#
  walk)

(define (bv->int bv)
  (if (= 0 (bytevector-length bv))
    0
    (let ((l (reverse (bytevector->u8-list bv))))
      (fold-left
        (^[cur e] (+ (* cur 256) e))
        (car l)
        (cdr l)))))

(define (bv->str bv)
  (define l (bytevector->u8-list bv))
  (define (itr l)
    (if (pair? l)
      (if (= (car l) 0)
        '()
        (cons (car l)
              (itr (cdr l))))
      '()))
  (list->string (map integer->char (itr l))))


)
