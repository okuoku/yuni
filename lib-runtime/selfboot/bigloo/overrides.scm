(define-syntax %%yuni/define-compar
  (syntax-rules ()
    ((_ comp nam)
     (begin
       (define (nam a b . args)
         (and (comp a b)
              (or (null? args)
                  (apply nam a args))))
       (yuni/base-library-add-var! 'nam 'comp)))))

(%%yuni/define-compar char=? char=?+)
(%%yuni/define-compar char<=? char<=?+)
(%%yuni/define-compar char>=? char>=?+)
(%%yuni/define-compar char<? char<?+)
(%%yuni/define-compar char>? char>?+)

(define (%%assoc/compare key alist compare)
  (cond
    ((pair? alist)
     (let ((a (car alist))
           (d (cdr alist)))
       (if (compare (car a) key)
         a
         (%%assoc/compare key d compare))))
    ((null? alist) #f)
    (else
      (error 'assoc/compare "Unknown object" alist))))

(define (assoc+ key alist . =?)
  (if (null? =?)
    (assoc key alist)
    (%%assoc/compare key alist (car =?))))

(define (%%member/compare key lis compare)
  (cond
    ((pair? lis)
     (if (compare (car lis) key)
       lis
       (%%member/compare key (cdr lis) compare)))
    ((null? lis) #f)
    (else
      (error 'member/compare "Unknown object" lis))))

(define (member+ key lis . =?)
  (if (null? =?)
    (member key lis)
    (%%member/compare key lis (car =?))))

(yuni/base-library-add-var! 'assoc+ 'assoc)
(yuni/base-library-add-var! 'member+ 'member)

;; FIXME: Not effective..?
(define string->list+
  (case-lambda
    ((x) (string->list x))
    ((x start) (string->list (substring x start (string-length x))))
    ((x start end) (string->list (substring x start end)))))

(yuni/base-library-add-var! 'string->list+ 'string->list)

(define (%%yunierror msg irr)
  (error #f msg irr))

(yuni/base-library-add-var! '%%yunierror 'error)
(yuni/base-library-add-var! 'yuni/command-line 'command-line)

(define (log+ x . y?)
  (if (null? y?)
    (log x)
    (/ (log x) (log (car y?)))))

(yuni/base-library-add-var! 'log+ 'log)

