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
(define yuni/biwas/YuniFileInput (js-ref yuni/biwas/Port "YuniFileInput"))
(define yuni/biwas/YuniFileOutput (js-ref yuni/biwas/Port "YuniFileOutput"))
(define yuni/biwas/YuniBinaryFileInput 
  (js-ref yuni/biwas/Port "YuniFileBinaryInput"))
(define yuni/biwas/YuniBinaryFileOutput 
  (js-ref yuni/biwas/Port "YuniFileBinaryOutput"))

(define (yuni/fs-open fs path flags) ;; => (err . fd)
  (let ((x (yuni/js-invoke/async fs "open" path flags)))
   (write x) (newline)
   (let* ((err (vector-ref x 0))
          (fd (and (js-null? err) (vector-ref x 1))))
     (cons err fd))))

(define (yuni/openfile cls mode file)
  (let ((fil (yuni/fs-open yuni/stdfs file mode)))
    (let ((err (car fil))
          (fd (cdr fil)))
      (cond
        ((js-null? err)
         ;; Success
         (js-new cls yuni/stdfs fd))
        (else
          (write err) (newline)
          (error "File open error" file err))))))

(define (open-input-file file) 
  (yuni/openfile yuni/biwas/YuniFileInput "r" file))
(define (open-output-file file) 
  (yuni/openfile yuni/biwas/YuniFileOutput "w+" file))
(define (open-binary-input-file file)
  (yuni/openfile yuni/biwas/YuniBinaryFileInput "r" file))
(define (open-binary-output-file file)
  (yuni/openfile yuni/biwas/YuniBinaryFileOutput "w+" file))
