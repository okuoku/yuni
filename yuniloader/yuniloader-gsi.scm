(include "./yuniloader-fake.scm")
(include "../external/yuni-alexpander.scm")

(define %%myenv (%%yuniloader-alexpander-newenv))
(define (%%expand frm)
  (%%yuniloader-alexpander-expand-top-level-forms!
    frm
    %%myenv))


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
   (let ((fn (string-append modpath "/yuniffi-gambit")))
    (load fn))
   (cond
     (do-dump
       (pp (%%expand code))
       (exit 0))
     (else
       (eval (cons 'begin (filtnull (%%expand code)))))))
 (define cmd (command-line))

 (%%yuniloader-fake-generate (cddr cmd) runner))
