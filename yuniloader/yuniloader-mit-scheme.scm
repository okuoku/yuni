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



(let ()
 (define (%%expand frm)
   (define %%myenv (%%yuniloader-alexpander-newenv))
   (%%yuniloader-alexpander-expand-top-level-forms!
     frm
     %%myenv))

 (define (eval-core frm)
   (eval frm (nearest-repl/environment)))
 (define (runner code arg* modpath do-dump)
   (define (filtnull code)
     (let loop ((acc '()) (cur code))
      (if (pair? cur)
        (let ((a (car cur))
              (d (cdr cur)))
          (if (null? a)
            (loop acc d)
            (loop (cons a acc) d)))
        (reverse acc))))
   (let ((expanded (cons 'begin (filtnull (%%expand code)))))
    (cond
      (do-dump
        (pp expanded)))
    (eval-core expanded)))

 (define cmd (command-line))

 (cond
   ((and (list? cmd) (string=? (car cmd) "-YUNIROOT"))
    (let ((root (cadr cmd))
          (next (cddr cmd)))
      (set! %%yunirootpath root)
      (set! cmd next))))

 (load (%%yuniroot "yuniloader/yuniloader-fake.scm"))
 (load (%%yuniroot "external/yuni-alexpander.scm"))

 (for-each eval-core %%yuniloader-alexpander-init)

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
