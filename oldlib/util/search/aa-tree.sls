(library (yuni util search aa-tree)
         (export aa-tree-init
                 aa-tree-insert
                 aa-tree-search-nearest)
         (import (rnrs))

;;

;; for-performance reasons, we use a vector here...

;; Node := #(value obj level left right)
(define (node-set-value! n value obj)
  (vector-set! n 0 value)
  (vector-set! n 1 obj))
(define (node-set-level! n obj)
  (vector-set! n 2 obj))
(define (node-set-left! n obj)
  (vector-set! n 3 obj))
(define (node-set-right! n obj)
  (vector-set! n 4 obj))

(define (node-value n)
  (vector-ref n 0))
(define (node-data n)
  (vector-ref n 1))
(define (node-level n)
  (vector-ref n 2))
(define (node-left n)
  (vector-ref n 3))
(define (node-right n)
  (vector-ref n 4))

(define (node-new value obj)
  (define me (vector '() '() '() '() '()))
  (node-set-value! me value obj)
  (node-set-level! me 0)
  me)

(define (aa-tree-init)
  '())

(define (skew t)
  (cond
    ((null? t)
     t)
    ((null? (node-left t))
     t)
    ((= (node-level (node-left t))
        (node-level t))
     (let ((L (node-left t)))
       (node-set-left! t (node-right L))
       (node-set-right! L t)
       L))
    (else t)))

(define (split t)
  (cond
    ((null? t) t)
    ((or (null? (node-right t)) (null? (node-right (node-right t))))
     t)
    ((= (node-level t) (node-level (node-right (node-right t))))
     (let ((R (node-right t)))
       (node-set-right! t (node-left R))
       (node-set-left! R t)
       (node-set-level! R (+ 1 (node-level R)))
       R))
    (else t)))

(define (aa-tree-insert t value obj)
  ;; update tree
  (cond
    ((null? t) 'do-nothing)
    ((< value (node-value t))
     (node-set-left! t (aa-tree-insert (node-left t) value obj)))
    ((> value (node-value t))
     (node-set-right! t (aa-tree-insert (node-right t) value obj)))
    (else
      ;; FIXME: Handle error
      #|
      (assertion-violation 'aa-tree-insert
                           "Invalid value"
                           value
                           obj)
      |#
      'do-nothing
      ))

  ;; Return
  (if (null? t)
    (node-new value obj)  
    (split (skew t))))

(define (aa-tree-search-nearest t value) ;; => l-value l-obj r-value r-obj
  (define (finish L R)
    (cond
      ((< value (node-value L))
       (values #f #f (node-value L) (node-data L)))
      ((< (node-value R) value)
       (values (node-value R) (node-data R) #f #f))
      (else
        (values (node-value L) (node-data L) (node-value R) (node-data R)))))

  (define (finish* a b) ;; unordered finish
    (if (< (node-value a) (node-value b))
      (finish a b)
      (finish b a)))

  (define (itr parent t)
    (cond
      ((null? t) (values #f #f #f #f))
      (else
        ;(write (list 'itr: (node-value t) (node-data t)))(newline)
        (let ((L (node-left t))
              (R (node-right t)))
          (cond
            ;; Finish (only one node)
            ((and (null? L) (null? R) (null? parent))
             ;; = Only one node in tree
             (if (< value (node-value t))
               (values #f #f (node-value t) (node-data t))
               (values (node-value t) (node-data t) #f #f)))
            ;; Finish
            ((and (null? L) (null? R))
             (finish* parent t))
            ;; go left or right
            (else
              (if (< value (node-value t))
                (cond ;; Check left
                  ((null? L) (finish* parent t))
                  ((< (node-value L) value) (finish* L t))
                  (else (itr t L)))
                (cond ;; Check right
                  ((null? R) (finish* parent t))
                  ((< value (node-value R)) (finish* R t))
                  (else (itr t R))))))))))
  (itr '() t))

)
