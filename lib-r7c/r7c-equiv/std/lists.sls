(library (r7c-equiv std lists)
         (export assoc
                 member)
         (import (r7c-basic syntax define)
                 (r7c-system core)
                 (r7c core error)
                 (r7c syntax if)
                 (r7c syntax cond)
                 (r7c heap pair)
                 (r7c-equiv std equal))

(define (member/equal obj lis)
  (cond
    ((pair? lis)
     (if (equal? obj (car lis))
       lis
       (member/equal obj (cdr lis))))
    ((null? lis) #f)
    (else (error "Unexpected."))))
         
(define (member/compare obj lis compare)
  (cond
    ((pair? lis)
     (if (compare obj (car lis))
       lis
       (member/compare obj (cdr lis) compare)))
    ((null? lis) #f)
    (else (error "Unexpected."))))
         
(define (member obj lis . compare?)
  (if (null? compare?)
    (member/equal obj lis)
    (member/compare obj lis (car compare?))))

(define (assoc/equal obj alist)
  (cond
    ((pair? alist)
     (let ((a (car alist))
           (d (cdr alist)))
       (if (equal? (car a) obj)
         a
         (assoc/equal obj d))))
    ((null? alist) #f)
    (else (error "Unexpected."))))
         
(define (assoc/compare obj alist compare)
  (cond
    ((pair? alist)
     (let ((a (car alist))
           (d (cdr alist)))
       (if (compare (car a) obj)
         a
         (assoc/compare obj d compare))))
    ((null? alist) #f)
    (else (error "Unexpected."))))
         
(define (assoc obj alist . compare?)
  (if (null? compare?)
    (assoc/equal obj alist)
    (assoc/compare obj alist (car compare?))))
         
)
