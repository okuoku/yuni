;; ~/local/mit-scheme/bin/mit-scheme --batch-mode 
;; --load yuniloader/yuniloader-mit-scheme.scm 
;; -- -YUNIROOT $PWD -I lib -I lib-compat -I lib-stub/gen 
;; -I lib-stub/mit-scheme -PROG _sanity.sps
;; 

;; current-error-port (which is required by yuniloader-fake)
(define (current-error-port) (trace-output-port))

(define %%yuniloader-result 0)

(define (exit . arg) ;; override
  (if (null? arg)
    (exit 0)
    (let ((x (car arg)))
     (cond
       ((boolean? x)
        (if x
          (set! %%yuniloader-result 0)
          (set! %%yuniloader-result 1)))
       (else
         (set! %%yuniloader-result x)))
     (%exit %%yuniloader-result))))

(define %%yunirootpath #f)

(define (%%yuniroot str)
  (if %%yunirootpath
    (string-append %%yunirootpath "/" str)
    str))

(load (%%yuniroot "yuniloader/yuniloader-fake.scm"))
(load (%%yuniroot "external/yuni-alexpander.scm"))


(let ()
 (define %%myenv (%%yuniloader-alexpander-newenv))
 (define (%%expand frm)
   (%%yuniloader-alexpander-expand-top-level-forms!
     frm
     %%myenv))

 (define (eval-core frm)
   (eval frm (nearest-repl/environment)))
 (define (runner code arg* modpath)
   (define (filtnull code)
     (let loop ((acc '()) (cur code))
      (if (pair? cur)
        (let ((a (car cur))
              (d (cdr cur)))
          (if (null? a)
            (loop acc d)
            (loop (cons a acc) d)))
        (reverse acc))))
   (eval-core (cons 'begin (filtnull (%%expand code)))))

 (for-each eval-core %%yuniloader-alexpander-init)
 (define cmd (command-line))

 (cond
   ((and (list? cmd) (string=? (car cmd) "-YUNIROOT"))
    (let ((root (cadr cmd))
          (next (cddr cmd)))
      (set! %%yuniroot root)
      (set! cmd next))))

 (fluid-let 
   ((standard-error-hook (lambda (c) 
                           (display "ERROR!\n" (trace-output-port))
                           (pp c)
                           (write-condition-report c
                                                   (trace-output-port))
                           (newline (trace-output-port))
                           (%exit 1))))
   (%%yuniloader-fake-generate cmd runner)))

(%exit %%yuniloader-result)
