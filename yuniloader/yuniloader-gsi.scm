(include "./yuniloader-fake.scm")
(include "../external/yuni-alexpander.scm")

(define %%myenv (%%yuniloader-alexpander-newenv))
(define (%%expand frm)
  (%%yuniloader-alexpander-expand-top-level-forms!
    frm
    %%myenv))

(define 
  %%yuniloader-macros
  '((define-macro (define-values frm expr)
      (define (flatten frm)
        (cond
          ((pair? frm)
           (cons
             (cons (car frm) (gensym (car frm)))
             (flatten (cdr frm))))
          ((null? frm) '())
          (else (list (cons frm (gensym frm))))))
      (define (dotted-vars frm)
        (if (null? (cddr frm))
          (cons (cdr (car frm)) (cdr (cadr frm)))
          (cons (cdr (car frm)) (dotted-vars (cdr frm)))))
      (define dotted? (and (pair? frm) (not (list? frm))))
      (define single? (not (pair? frm)))
      (let ((vars (flatten frm))
            (bogus (gensym 'bogus)))
       `(begin
          ,@(map (lambda (v) `(define ,(car v) #f)) vars)
          (define ,bogus
            (call-with-values
              (lambda () ,expr)
              ,(cond
                 (dotted?
                   `(lambda ,(dotted-vars vars)
                      ,@(map (lambda (v) `(set! ,(car v) ,(cdr v))) vars)))
                 (single?
                   `(lambda ,(cdr (car vars))
                      (set! ,frm ,(cdr (car vars)))))
                 (else
                   `(lambda ,(map cdr vars)
                      ,@(map (lambda (v) 
                               `(set! ,(car v) ,(cdr v))) vars)))))))))))


(define %%yuniffi-gambit-modpath #f)

(let ()
 #|
 ;; Slow runner (for debugging)
 (define (runner code arg* modpath)
   (define (run frm)
     (if (eq? 'begin (car frm))
       (for-each run (cdr frm))
       (begin
         ;(pp frm)
         (let ((runfrm (%%expand frm)))
          ;(pp "==================================")   
          (cond
            ((not (null? runfrm))
             ;(pp runfrm)
             (eval (cons 'begin runfrm)))))
         ;(pp "==================================")
         )))
   (set! %%yuniffi-gambit-modpath modpath)
   (for-each eval %%yuniloader-alexpander-init)
   (let ((fn (string-append modpath "/yuniffi-gambit")))
    (load fn))
   (for-each run code))
 |#

 (define do-slow-eval #f)
 (define (do-eval form)
   (pp "==================================")
   (pp form)
   (pp "==================================")
   (eval form))
 (define (runner code arg* modpath do-dump)
   (define (filtnull code)
     (let loop ((acc '()) (cur code))
      (if (pair? cur)
        (let ((a (car cur))
              (d (cdr cur)))
          (if (null? a)
            (loop acc d)
            (loop (cons a acc) d)))
        (reverse acc)) ))
   (set! %%yuniffi-gambit-modpath modpath)
   (for-each eval %%yuniloader-alexpander-init)
   (for-each eval %%yuniloader-macros)
   (let ((fn (string-append modpath "/yuniffi-gambit")))
    (load fn))
   (cond
     (do-dump
       (pp (%%expand code))
       (exit 0))
     (do-slow-eval
       (for-each do-eval (filtnull (%%expand code)))
       (exit 0)
       )
     (else
       (eval (cons 'begin (filtnull (%%expand code))))
       (exit 0))))
 (define cmd (command-line))

 (%%yuniloader-fake-generate (cddr cmd) runner))
