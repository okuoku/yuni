(library (r7c-basic lib lists)
         (export
           list?
           list
           append
           reverse
           memq
           assq
           assv
           make-list
           length
           list-tail
           list-ref
           list-set!
           list-copy)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c core apply)
                 (r7c core error)
                 (r7c syntax if)
                 (r7c syntax or)
                 (r7c syntax and)
                 (r7c syntax let)
                 (r7c syntax cond)
                 (r7c syntax unless)
                 (r7c heap core)
                 (r7c heap eqv)
                 (r7c heap fixnum)
                 (r7c heap pair))
         

(define (list? obj)
  (or (null? obj)
      (and (pair? obj)
           (list? (cdr obj)))))

(define (list . x) x)

(define (append/itr! cur lis queue)
  (cond
    ((null? lis)
     (let ((a (car queue))
           (d (cdr queue)))
       (cond
         ((null? d)
          ;; Terminate
          (set-cdr! cur a))
         (else
           ;; Go next
           (append/itr! cur a d)))))
    (else
      (let ((c (cons (car lis) '())))
       (set-cdr! cur c)
       (append/itr! c (cdr lis) queue)) ))) 

(define (append . args)
  (cond
    ;; 0 args
    ((null? args) '())
    ;; 1 arg
    ((null? (cdr args)) (car args))
    ;; 2+ args: null?
    ((null? (car args)) (apply append (cdr args)))
    ;; 2 args
    ((null? (cddr args)) ($append (car args) (cadr args)))
    ;; 2+ args
    (else
      (let* ((aa (caar args))
             (da (cdar args))
             (b (cdr args))
             (c (cons aa '())))
        (append/itr! c da b)
        c))))

(define (reverse/itr cur lis)
  (cond
    ((pair? lis)
     (let ((a (car lis))
           (d (cdr lis)))
       (reverse/itr (cons a cur) d)))
    ((null? lis)
     cur)
    (else
      (error "List required" lis))))

(define (reverse x)
  (reverse/itr '() x))

(define (memq obj list)
  (cond
    ((pair? list)
     (if (eq? obj (car list))
       list
       (memq obj (cdr list))))
    ((null? list) #f)
    (else
      (error "List required" list))))

#|
(define (memv obj list)
  (cond
    ((pair? list)
     (if (eqv? obj (car list))
       list
       (memv obj (cdr list))))
    ((null? list) #f)
    (else
      (error "List required" list))))
|#

(define (assq obj alist)
  (cond
    ((pair? alist)
     (let ((a (car alist))
           (d (cdr alist)))
       (unless (pair? a)
         (error "Invalid alist entry" a))
       (if (eq? (car a) obj)
         a
         (assq obj d))))
    ((null? alist) #f)
    (else
      (error "alist required" alist)))) 

(define (assv obj alist)
  (cond
    ((pair? alist)
     (let ((a (car alist))
           (d (cdr alist)))
       (unless (pair? a)
         (error "Invalid alist entry" a))
       (if (eqv? (car a) obj)
         a
         (assv obj d))))
    ((null? alist) #f)
    (else
      (error "alist required" alist)))) 

(define (make-list/fill cur k fil)
  (if ($fx>= 0 k)
    cur
    (make-list/fill (cons fil cur) ($fx- k 1) fil)))

(define (make-list k . fill?)
  (if (null? fill?)
    (make-list/fill '() k #f)
    (make-list/fill '() k (car fill?))))

(define length $fx-length)

(define (list-tail lis k)
  (if ($fx>= 0 k)
    lis
    (list-tail (cdr lis) ($fx- k 1))))

(define (list-ref lis k)
  (car (list-tail lis k)))

(define (list-set! lis k v)
  (set-car! (list-tail lis k) v))

(define (list-copy/itr! cur lis)
  (cond
    ((pair? lis)
     (let ((c (cons (car lis) '())))
      (set-cdr! cur c)
      (list-copy/itr! c (cdr lis))))
    (else
      (set-cdr! cur lis))))

(define (list-copy obj)
  (if (pair? obj)
    (let ((c (cons (car obj) '())))
     (list-copy/itr! c (cdr obj))
     c)
    obj))

)
