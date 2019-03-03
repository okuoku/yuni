(define *yuni/libalias*
  '((yuni . biwascheme-yuni)
    (scheme . biwascheme-compat-scheme)))

(define (yuni/gensym bogus) (gensym))
(define (yuni/identifier? x) (symbol? x))
(define yuni/args (command-line))
(define (yuni/command-line) yuni/args)
(define (yuni/update-command-line! lis) (set! yuni/args lis))
(define (exact x) (truncate x))
(define (inexact x) x)
(define (inexact? x) #t)
(define (exact? x) (= x (truncate x)))

;; Extra libraries
(define yuni/stdfs (yuni/js-import "fs"))
(define yuni/biwascheme (yuni/js-import "biwascheme"))
(define yuni/biwas/Port (js-ref yuni/biwascheme "Port"))
(define yuni/biwas/YuniFileInput (js-ref yuni/biwas/Port "YuniFileInput"))
(define yuni/biwas/YuniFileOutput (js-ref yuni/biwas/Port "YuniFileOutput"))
(define yuni/biwas/YuniBinaryFileInput 
  (js-ref yuni/biwas/Port "YuniFileBinaryInput"))
(define yuni/biwas/YuniBinaryFileOutput 
  (js-ref yuni/biwas/Port "YuniFileBinaryOutput"))

;; R7RS Ports

(define (yuni/fs-open fs path flags) ;; => (err . fd)
  (let ((x (yuni/js-invoke/async fs "open" path flags)))
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

(define write-u8
  (case-lambda
    ((b) (write-u8 b (current-output-port)))
    ((b port) (write-bytevector (bytevector b) port))))

(define read-bytevector
  (case-lambda
    ((k) (read-bytevector k (current-input-port)))
    ((k port)
     (let ((bv (make-bytevector k)))
      (let ((r (read-bytevector! bv port)))
       (if (eof-object? r)
         (eof-object)
         bv))))))

(define read-u8
  (case-lambda
    (() (read-u8 (current-input-port)))
    ((port)
     (let ((bv (read-bytevector 1 port)))
      (if (eof-object? bv)
        (eof-object)
        (bytevector-u8-ref bv 0))))))

;; R7RS substring-able procedures

(define (%substring1 str start) (substring str start (string-length str)))

(define string->list
  (case-lambda
    ((str) (r6:string->list str))
    ((str start) (r6:string->list (%substring1 str start)))
    ((str start end) (r6:string->list (substring str start end)))))

(define string-copy
  (case-lambda
    ((str) (r6:string-copy str))
    ((str start) (%substring1 str start))
    ((str start end) (substring str start end))))

(define string->vector
  (case-lambda
    ((str) (list->vector (string->list str)))
    ((str start) (string->vector (%substring1 str start)))
    ((str start end) (string->vector (substring str start end)))))

(define (string-map p . args) 
  (apply string 
         (apply map p 
                (map string->list args))))

;; R7RS iters
(define assoc
  (case-lambda
    ((obj alist) (r6:assoc obj alist))
    ((obj alist compare) (assp (lambda (x) (compare obj x)) alist))))

(define member
  (case-lambda
    ((obj alist) (r6:member obj alist))
    ((obj alist compare) (memp (lambda (x) (compare obj x)) alist))))

;; R7RS vectors

(define (%subvector v start end)
  (define mlen (- end start))
  (define out (make-vector (- end start)))
  (define (itr r)
    (if (= r mlen)
      out
      (begin
        (vector-set! out r (vector-ref v (+ start r)))
        (itr (+ r 1)))))
  (itr 0))

(define (%subvector1 v start) (%subvector v start (vector-length v)))

(define vector->string
  (case-lambda
    ((v) (list->string (vector->list v)))
    ((v start) (vector->string (%subvector1 v start)))
    ((v start end) (vector->string (%subvector v start end)))))

(define vector-copy
  (case-lambda
    ((v) (r6:vector-copy v))
    ((v start) (%subvector1 v start))
    ((v start end) (%subvector v start end))))

(define (%vector-copy!-neq to at from start end)
  (define term (+ at (- end start)))
  (define (itr r)
    (unless (= r term)
      (vector-set! to r (vector-ref from (+ start (- r at))))
      (itr (+ r 1))))
  (itr at))

(define vector-copy!
  (case-lambda
    ((to at from)
     (vector-copy! to at from 0 (vector-length from)))
    ((to at from start)
     (vector-copy! to at from start (vector-length from)))
    ((to at from start end)
     (if (eq? to from)
       (%vector-copy!-neq to at (%subvector from start end) 0 (- end start))
       (%vector-copy!-neq to at from start end)))))

(define vector-fill!
  (case-lambda
    ((vec fill) (r6:vector-fill! vec fill))
    ((vec fill start) (vector-fill! vec fill start (vector-length vec)))
    ((vec fill start end)
     (define (itr r)
       (unless (= r end)
         (vector-set! vec r fill)
         (itr (+ r 1))))
     (itr start))))


;; R7RS bytevectors
(define (%subbytevector bv start end)
  (define mlen (- end start))
  (define out (make-bytevector mlen))
  (r6:bytevector-copy! bv start out 0 mlen)
  out)

(define (%subbytevector1 bv start)
  (%subbytevector bv start (bytevector-length bv)))

(define bytevector-copy!
  (case-lambda
    ((to at from) (bytevector-copy! to at from 0))
    ((to at from start)
     (let ((flen (bytevector-length from))
           (tlen (bytevector-length to)))
       (let ((fmaxcopysize (- flen start))
             (tmaxcopysize (- tlen at)))
         (bytevector-copy! to at from start (+ start
                                               (min fmaxcopysize
                                                    tmaxcopysize))))))
    ((to at from start end)
     (r6:bytevector-copy! from start to at (- end start)))))

(define bytevector-copy
  (case-lambda
    ((bv) (r6:bytevector-copy bv))
    ((bv start) (%subbytevector1 bv start))
    ((bv start end) (%subbytevector bv start end))))

(define utf8->string
  (case-lambda
    ((bv) (r6:utf8->string bv))
    ((bv start) (r6:utf8->string (%subbytevector1 bv start)))
    ((bv start end) (r6:utf8->string (%subbytevector bv start end)))))

(define string->utf8
  (case-lambda
    ((str) (r6:string->utf8 str))
    ((str start) (r6:string->utf8 (%substring1 str start)))
    ((str start end) (r6:string->utf8 (substring str start end)))))

;; R7RS errors
(define (error . args)
  (raise (list args)))
