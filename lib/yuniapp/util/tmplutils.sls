(library (yuniapp util tmplutils)
         (export
           escape-path
           libname->string
           exportsyms->string
           sexp-list->lines
           filter-stdaux
           filter-keyword)

         (import (yuni scheme))

(define (escape-path pth)
  (define (itr acc cur)
    (if (pair? cur)
      (let ((c (car cur)))
       (if (char=? #\\ c)
         (itr (cons #\\ (cons #\\ acc)) (cdr cur))
         (itr (cons c acc) (cdr cur))))
      (reverse acc)))
  (list->string
    (itr '()
         (string->list pth))))

(define (libname->string libname)
  (define (itr acc cur)
    (if (pair? cur)
      (let ((a (symbol->string (car cur))))
       (itr (string-append acc " " a)
            (cdr cur)))
      (string-append acc ")")))
  (itr "(" libname))         

(define (exportsyms->string syms)
  (define (itr acc cur)
    (if (pair? cur)
      (itr (string-append acc (symbol->string (car cur)) "\n") (cdr cur))
      acc))
  (itr "" syms))

(define (%filter-syms proc syms)
  (define (itr acc cur)
    (if (pair? cur)
      (let ((a (car cur)))
       (if (proc a)
         (itr acc (cdr cur))
         (itr (cons a acc) (cdr cur))))
      acc))
  (itr '() syms))

(define (sexp-list->lines sexp)
  ;; FIXME: Non-portable
  (let ((p (open-output-string)))
   (for-each (lambda (e) (write e p) (newline p))
             sexp)
   (get-output-string p)))

(define (filter-stdaux syms)
  (%filter-syms (lambda (e) (memq e '(_ ... => else unquote unquote-splicing)))
                syms))

(define (filter-keyword syms)
  (%filter-syms (lambda (e) (char=? #\: (string-ref (symbol->string e) 0)))
                syms)) 
)
