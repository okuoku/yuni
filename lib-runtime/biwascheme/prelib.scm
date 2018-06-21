(define (yuni/gensym bogus) (gensym))
(define (yuni/identifier? x) (symbol? x))
(define yuni/args (cdddr (command-line))) ;; Drop /usr/bin/node, biwas itself
(define (yuni/command-line) yuni/args)
(define (yuni/update-command-line! lis) (set! yuni/args lis))
(define (exact x) (truncate x))
(define (inexact x) x)
(define (inexact? x) #t)
(define (exact? x) (= x (truncate x)))

;; Extra libraries
(define yuni/stdfs (js-eval "require('fs')"))
(define yuni/biwascheme (yuni/js-import "biwascheme"))
(define yuni/biwas/Port (js-ref yuni/biwascheme "Port"))
(define yuni/biwas/CustomInput (js-ref yuni/biwas/Port "CustomInput"))
(define yuni/biwas/CustomOutput (js-ref yuni/biwas/Port "CustomOutput"))

(define %define-invoke/async
  (js-eval
    (string-append
      "var x = function(biwas){biwas.define_libfunc(\"yuni/js-invoke/async\",2,null,function(ar){"
      "var js_obj = ar.shift(); var func_name = ar.shift();"
      "return new biwas.Pause(function(pause){"
      "var cb = function(){console.log(arguments); return pause.resume(arguments);};"
      "ar.push(cb);"
      "console.log(ar);"
      "js_obj[func_name].apply(js_obj, ar); }); });};x")))

(js-call %define-invoke/async yuni/biwascheme)

(define (yuni/fs-open fs path flags) ;; => (err . fd)
  (let ((x (yuni/js-invoke/async fs "open" path flags)))
   (write x) (newline)
   (let* ((v (vector-ref x 0))
          (err (vector-ref v 0))
          (fd (vector-ref v 1)))
     (cons err fd))))

#|
(define (open-input-file file)
  ;; FIXME: Incomplete
  (let ((fil (yuni/fs-open yuni/stdfs file "r")))
    (let ((err (car fil))
          (fd (cdr fil)))
      (write (cons err fd)) (newline)
      #f)))
|#
