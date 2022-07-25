(define-syntax %%yuni/define-compar
  (syntax-rules ()
    ((_ comp nam)
     (begin
       (define (nam a b . args)
         (and (comp a b)
              (or (null? args)
                  (apply nam b args))))
       (yuni/base-library-add-var! 'nam 'comp)))))

(%%yuni/define-compar char=? char=?+)
(%%yuni/define-compar char<=? char<=?+)
(%%yuni/define-compar char>=? char>=?+)
(%%yuni/define-compar char<? char<?+)
(%%yuni/define-compar char>? char>?+)
(%%yuni/define-compar string=? string=?+)
(%%yuni/define-compar string<? string<?+)
(%%yuni/define-compar string<=? string<=?+)
(%%yuni/define-compar string>? string>?+)
(%%yuni/define-compar string>=? string>=?+)

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

(define string->list+
  (case-lambda
    ((x) (string->list x))
    ((x start) (string->list (substring x start (string-length x))))
    ((x start end) (string->list (substring x start end)))))

(yuni/base-library-add-var! 'string->list+ 'string->list)

;; FIXME: Copy/Pasted from polyfills
(define string-copy+
  (case-lambda
    ((s) (string-copy s))
    ((s start) (string-copy+ s start (string-length s)))
    ((s start end) (list->string (string->list+ s start end)))))

(yuni/base-library-add-var! 'string-copy+ 'string-copy)

(define (%%yunierror msg irr)
  (error #f msg irr))

(yuni/base-library-add-var! '%%yunierror 'error)
(yuni/base-library-add-var! 'yuni/command-line 'command-line)

(define (log+ x . y?)
  (if (null? y?)
    (log x)
    (/ (log x) (log (car y?)))))

(yuni/base-library-add-var! 'log+ 'log)

(yuni/base-library-add-var! 'read-chars 'read-string)

(define write-string/yuni
  (case-lambda
    ((string) (write-string/yuni string (current-output-port)
                                 0 (string-length string)))
    ((string port)
     (write-string/yuni string port 0 (string-length string)))
    ((string port start)
     (write-string/yuni string port start (string-length string)))
    ((string port start end)
     (unless (= start end)
       (write-char (string-ref string start) port)
       (write-string/yuni string port (+ 1 start) end)) )))

(yuni/base-library-add-var! 'write-string/yuni 'write-string)
(yuni/base-library-add-var! 'port? 'binary-port?)

(define (exit/yuni arg)
  ;; Bigloo exit will success on (exit #f)
  (if arg
    (exit arg)
    (exit 1)))

(yuni/base-library-add-var! 'exit/yuni 'exit)
