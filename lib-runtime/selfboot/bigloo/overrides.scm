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

(define (%%yunierror msg irr)
  (error #f msg irr))

(yuni/base-library-add-var! 'char=?+ 'char=?)
(yuni/base-library-add-var! '%%yunierror 'error)
(yuni/base-library-add-var! 'yuni/command-line 'command-line)

