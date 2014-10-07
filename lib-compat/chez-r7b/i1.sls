(library (chez-r7b i1)
         (export map for-each member assoc list-copy)   
         (import (rename (chezscheme)
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

)
