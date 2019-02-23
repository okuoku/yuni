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

;; Generate renamer for __1 literals and base-syntax hook
(define (yuni/make-synrule-baselib)  ;; => ^(sym)
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
        (let ((s (do-gensym idx)))
         (set! cache-index idx)
         (set! cache s)
         s))))
  (lambda (sym)
    (let ((i (yuni/reserved-underscore-index sym)))
     (or (and i (take i))
         sym))))

(define (yuni/synrule-compare x y) (eq? x y))

(define (yuni/synrule-prescan-lambda-frm ellipsis? frm)
  (define (itr l cur)
    (cond
      ((and (yuni/identifier? l) (not (ellipsis? l)))
       (cons l cur))
      ((pair? l)
       (let ((a (car l))
             (d (cdr l)))
         (cond 
           ((and (yuni/identifier? a) (not (ellipsis? a)))
            (itr d (cons a cur)))
           (else
             (itr d cur)))))
      (else cur)))
  (itr frm '()))

(define (yuni/synrule-prescan-lambda ellipsis? frm)
  (yuni/synrule-prescan-lambda-frm ellipsis? (cadr frm)))

(define (yuni/synrule-prescan-let ellipsis? frm)
  (define (itr bind* cur)
    (cond
      ((pair? bind*)
       (let ((a (car bind*))
             (d (cdr bind*)))
         (cond
           ((and (list? a) (yuni/identifier? (car a)))
            (itr d (cons (car a) cur)))
           (else
             (itr d cur)))))
      (else cur)))
  (cond
    ((and (yuni/identifier? (cadr frm)) (not (ellipsis? (cadr frm)))
          (pair? (cddr frm)))
     (cons (cadr frm) 
           (itr (caddr frm) '())))
    (else
      (itr (cadr frm) '()))))

(define (yuni/synrule-prescan-define ellipsis? frm)
  (yuni/synrule-prescan-lambda-frm ellipsis? (cadr frm)))

(define (yuni/synrule-prescan-template ellipsis? tmpl) ;; => (sym ...)
  ;; Scan syntax-rules template and emit "to-be-bound" symbols
  ;; 
  ;;  (let ((a 10) (b 20)) body ...) => (a b)
  ;;  (let loop ((a 10)) body ...) => (loop a)
  (define templates
    (list
      (list '(let letrec let* letrec*) yuni/synrule-prescan-let)
      (list '(define) yuni/synrule-prescan-define)
      (list '(lambda) yuni/synrule-prescan-lambda)))

  (define (match? sym* sym)
    (yuni/find1 (lambda (s) (eq? s sym)) sym*))
  (define (lookup sym)
    (yuni/find1 (lambda (e) (match? (car e) sym)) templates))
  (define (can-be-a-binder? frm)
    (define (itr cur cnt)
      (cond
        ((= cnt 2) #t)
        ((pair? cur)
         (itr (cdr cur) (+ 1 cnt)))
        (else #f)))
    (itr frm 0))
  (define ret '())
  (define (pass! frm)
    (cond
      ((pair? frm)
       (let ((sym (car frm)))
        (cond
          ((and (yuni/identifier? sym) (can-be-a-binder? frm))
           (let ((trans (lookup sym)))
            (cond
              (trans
                (let ((syms ((cadr trans) ellipsis? frm)))
                 (set! ret (append syms ret))))))))
        (cond
          ((list? frm)
           (for-each pass! frm)))))))
  (pass! tmpl)
  ret)

(define-macro (define-syntax name synrule)
  (let ((tran (yuni/syntax-rules-transformer 
                synrule 
                yuni/gensym (yuni/make-synrule-baselib) yuni/synrule-compare))
        (args (yuni/gensym 'args))
        (a (yuni/gensym 'output)))
    `(define-macro (,name . ,args)
       (let ((,a (,tran (cons ',name ,args)
                        yuni/gensym (yuni/make-synrule-baselib) yuni/synrule-compare)))
         ;(display (list 'OUT: ,a))
         ;(newline)
         ,a))))

(define (yuni/convert-let-syntax cur e)
  (cond
    ((pair? e)
     (let* ((nam (caar e))
            (tmpl (cadar e))
            (temp (yuni/gensym nam)))
       (yuni/convert-let-syntax
         (cons
           `(define-macro (,nam . args) (cons ,temp args))
           (cons
             `(define-syntax ,temp ,tmpl)
             cur))
         (cdr e))))
    (else (reverse cur))))

(define-macro (let-syntax frm . body)
  `(let () 
    ,@(yuni/convert-let-syntax '() frm)
    . ,body))
