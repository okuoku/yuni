(library (chez-r7b i1)
         (export map for-each member assoc list-copy)   
         (import (rename (except (chezscheme) list-copy)
                         (member r6:member)
                         (assoc r6:assoc)))
         
(define member
  (case-lambda
    ((x lis) (r6:member x lis))
    ((x lis eqv) (memp (lambda (e) (eqv x e)) lis))))

(define assoc
  (case-lambda
    ((key alist) (r6:assoc key alist))
    ((key alist eqv) (assp (lambda (e) (eqv key e)) alist))))

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
