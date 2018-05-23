;; Took from chibi-scheme 22f87f67ab24d42fe0872cb5ceea6ecafe8933ad

;; init-7.scm -- core library procedures for R7RS
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-rules


;; YUNI: Added 4th argument `baselib` to non-rename injector
;;       `rename` can be a `gensym` procedure
(define (yuni/syntax-rules-transformer expr rename baselib compare)
  (let ((ellipsis-specified? (yuni/identifier? (cadr expr)))
        (count 0)
        (_er-macro-transformer (baselib 'yuni/er-macro-transformer4))
        (_lambda (baselib 'lambda))      (_let (baselib 'let))
        (_begin (baselib 'begin))        (_if (baselib 'if))
        (_and (baselib 'and))            (_or (baselib 'or))
        (_eq? (baselib 'eq?))            (_equal? (baselib 'equal?))
        (_car (baselib 'car))            (_cdr (baselib 'cdr))
        (_cons (baselib 'cons))          (_pair? (baselib 'pair?))
        (_null? (baselib 'null?))        (_expr (rename 'expr))
        (_rename (rename 'rename))       (_compare (rename 'compare))
        ;; YUNI: _quote can be a quote for us ... perhaps
        (_quote (baselib 'quote))        (_apply (baselib 'apply))
        (_append (baselib 'append))      (_map (baselib 'map))
        (_vector? (baselib 'vector?))    (_list? (baselib 'list?))
        (_len (rename 'len))             (_length (baselib 'yuni/length*))
        (_- (baselib '-))   (_>= (baselib '>=))   (_error (baselib 'error))
        (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
        (_reverse (baselib 'reverse))
        (_vector->list (baselib 'vector->list))
        (_list->vector (baselib 'list->vector))
        (_cons3 (baselib 'yuni/cons-source))
        (_underscore (baselib '_))
        ;; YUNI: Additional procedures
        (_baselib (rename 'baselib))
        (identifier? yuni/identifier?)   (length* yuni/length*)
        (identifier->symbol yuni/identifier->symbol)
        (find yuni/find1)                (any yuni/any1)
        (%number->string number->string))
    (define ellipsis (if ellipsis-specified? (cadr expr) (baselib '...)))
    (define lits (if ellipsis-specified? (car (cddr expr)) (cadr expr)))
    (define forms (if ellipsis-specified? (cdr (cddr expr)) (cddr expr)))
    (define (next-symbol s)
      (set! count (+ count 1))
      (rename (string->symbol (string-append s (%number->string count)))))
    (define (expand-pattern pat tmpl)
      (let lp ((p (cdr pat))
               (x (list _cdr _expr))
               (dim 0)
               (vars '())
               (k (lambda (vars)
                    (list _cons (expand-template tmpl vars) #f))))
        (let ((v (next-symbol "v.")))
          (list
           _let (list (list v x))
           (cond
            ((identifier? p)
             (cond
              ((ellipsis-mark? p)
               (error "bad ellipsis" p))
              ((memq p lits)
               (list _and
                     ;; FIXME: _rename => _baselib
                     (list _compare v (list _baselib (list _quote p)))
                     (k vars)))
              ((compare p _underscore)
               (k vars))
              (else
               (list _let (list (list p v)) (k (cons (cons p dim) vars))))))
            ((ellipsis? p)
             (cond
              ((not (null? (cdr (cdr p))))
               (cond
                ((any (lambda (x) (and (identifier? x) (ellipsis-mark? x)))
                      (cddr p))
                 (error "multiple ellipses" p))
                (else
                 (let ((len (length* (cdr (cdr p))))
                       (_lp (next-symbol "lp.")))
                   `(,_let ((,_len (,_length ,v)))
                      (,_and (,_>= ,_len ,len)
                             (,_let ,_lp ((,_ls ,v)
                                          (,_i (,_- ,_len ,len))
                                          (,_res (,_quote ())))
                                    (,_if (,_>= 0 ,_i)
                                        ,(lp `(,(cddr p)
                                               (,(car p) ,(car (cdr p))))
                                             `(,_cons ,_ls
                                                      (,_cons (,_reverse ,_res)
                                                              (,_quote ())))
                                             dim
                                             vars
                                             k)
                                        (,_lp (,_cdr ,_ls)
                                              (,_- ,_i 1)
                                              (,_cons3 (,_car ,_ls)
                                                       ,_res
                                                       ,_ls))))))))))
              ((identifier? (car p))
               (list _and (list _list? v)
                     (list _let (list (list (car p) v))
                           (k (cons (cons (car p) (+ 1 dim)) vars)))))
              (else
               (let* ((w (next-symbol "w."))
                      (_lp (next-symbol "lp."))
                      (new-vars (all-vars (car p) (+ dim 1)))
                      (ls-vars (map (lambda (x)
                                      (next-symbol
                                       (string-append
                                        (symbol->string
                                         (identifier->symbol (car x)))
                                        "-ls")))
                                    new-vars))
                      (once
                       (lp (car p) (list _car w) (+ dim 1) '()
                           (lambda (_)
                             (cons
                              _lp
                              (cons
                               (list _cdr w)
                               (map (lambda (x l)
                                      (list _cons (car x) l))
                                    new-vars
                                    ls-vars)))))))
                 (list
                  _let
                  _lp (cons (list w v)
                            (map (lambda (x) (list x (list _quote '()))) ls-vars))
                  (list _if (list _null? w)
                        (list _let (map (lambda (x l)
                                          (list (car x) (list _reverse l)))
                                        new-vars
                                        ls-vars)
                              (k (append new-vars vars)))
                        (list _and (list _pair? w) once)))))))
            ((pair? p)
             (list _and (list _pair? v)
                   (lp (car p)
                       (list _car v)
                       dim
                       vars
                       (lambda (vars)
                         (lp (cdr p) (list _cdr v) dim vars k)))))
            ((vector? p)
             (list _and
                   (list _vector? v)
                   (lp (vector->list p) (list _vector->list v) dim vars k)))
            ((null? p) (list _and (list _null? v) (k vars)))
            (else (list _and (list _equal? v p) (k vars))))))))
    (define ellipsis-mark?
      (if (if ellipsis-specified?
              (memq ellipsis lits)
              (any (lambda (x) (compare ellipsis x)) lits))
          (lambda (x) #f)
          (if ellipsis-specified?
              (lambda (x) (eq? ellipsis x))
              (lambda (x) (compare ellipsis x)))))
    (define (ellipsis-escape? x) (and (pair? x) (ellipsis-mark? (car x))))
    (define (ellipsis? x)
      (and (pair? x) (pair? (cdr x)) (ellipsis-mark? (cadr x))))
    (define (ellipsis-depth x)
      (if (ellipsis? x)
          (+ 1 (ellipsis-depth (cdr x)))
          0))
    (define (ellipsis-tail x)
      (if (ellipsis? x)
          (ellipsis-tail (cdr x))
          (cdr x)))
    (define (all-vars x dim)
      (let lp ((x x) (dim dim) (vars '()))
        (cond ((identifier? x)
               (if (or (memq x lits)
                       (compare x _underscore))
                   vars
                   (cons (cons x dim) vars)))
              ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
              ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
              ((vector? x) (lp (vector->list x) dim vars))
              (else vars))))
    (define (free-vars x vars dim)
      (let lp ((x x) (free '()))
        (cond
         ((identifier? x)
          (if (and (not (memq x free))
                   ;(cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                   ;      (else #f))
                   (let ((cell (assq x vars)))
                    (and cell
                         (>= (cdr cell) dim))))
              (cons x free)
              free))
         ((pair? x) (lp (car x) (lp (cdr x) free)))
         ((vector? x) (lp (vector->list x) free))
         (else free))))
    (define (expand-template tmpl vars)
      (let lp ((t tmpl) (dim 0))
        (cond
         ((identifier? t)
          ;(cond
          ; ((find (lambda (v) (eq? t (car v))) vars)
          ;  => (lambda (cell)
          ;       (if (<= (cdr cell) dim)
          ;           t
          ;           (error "too few ...'s"))))
          ; (else
          ;  (list _baselib (list _quote t))))
          
          (let ((cell (find (lambda (v) (eq? t (car v))) vars)))
           (if cell
             (if (<= (cdr cell) dim)
               t
               (error "two few ...'s"))
             (list _baselib (list _quote t)))))
         ((pair? t)
          (cond
           ((ellipsis-escape? t)
            (list _quote
                  (if (pair? (cdr t))
                      (if (pair? (cddr t)) (cddr t) (cadr t))
                      (cdr t))))
           ((ellipsis? t)
            (let* ((depth (ellipsis-depth t))
                   (ell-dim (+ dim depth))
                   (ell-vars (free-vars (car t) vars ell-dim)))
              (cond
               ((null? ell-vars)
                (error "too many ...'s"))
               ((and (null? (cdr (cdr t))) (identifier? (car t)))
                ;; shortcut for (var ...)
                (lp (car t) ell-dim))
               (else
                (let* ((once (lp (car t) ell-dim))
                       (nest (if (and (null? (cdr ell-vars))
                                      (identifier? once)
                                      (eq? once (car vars)))
                                 once ;; shortcut
                                 (cons _map
                                       (cons (list _lambda ell-vars once)
                                             ell-vars))))
                       (many (do ((d depth (- d 1))
                                  (many nest
                                        (list _apply _append many)))
                                 ((= d 1) many))))
                  (if (null? (ellipsis-tail t))
                      many ;; shortcut
                      (list _append many (lp (ellipsis-tail t) dim))))))))
           (else (list _cons3 (lp (car t) dim) (lp (cdr t) dim) (list _quote t)))))
         ((vector? t) (list _list->vector (lp (vector->list t) dim)))
         ((null? t) (list _quote '()))
         (else t))))
    (list
     _er-macro-transformer
     (list _lambda (list _expr _rename _baselib _compare)
           (list
            _car
            (cons
             _or
             (append
              (map
               (lambda (clause) (expand-pattern (car clause) (cadr clause)))
               forms)
              (list
               (list _cons
                     (list _error "no expansion for"
                           (list _expr))
                     #f)))))))))
