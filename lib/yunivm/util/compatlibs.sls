(library (yunivm util compatlibs)
         (export 
           compatlibs-proc-vector
           compatlibs-name-vector)
         (import (yuni scheme)
                 (yuni compat simple-struct)) 

(define (pexact proc)
  (lambda args (exact (apply proc args))))
(define (pexact2 proc)
  (lambda args
    (call-with-values (lambda () (apply proc args))
                      (lambda (a b) (values (exact a) (exact b))))))
(define (pinexact proc)
  (lambda args (inexact (apply proc args))))
(define (pinexact2 proc)
  (lambda args
    (call-with-values (lambda () (apply proc args))
                      (lambda (a b) (values (inexact a) (inexact b))))))
         
(define ($undefined) (if #f #f))
(define $append append)
(define $fx-length length)
(define $fx= =)
(define $fx<= <=)
(define $fx>= >=)
(define $fx< <)
(define $fx> >)
(define $fx+ (pexact +))
(define $fx- (pexact -))
(define $fx* (pexact *))
(define $fx/ (pexact /))
(define $fx->fl inexact)
(define $fx-expt (pexact expt))
(define $fx-floor/ (pexact2 floor/))
(define $fx-truncate/ (pexact2 truncate/))

(define $fl-nan? nan?)
(define $fl-finite? finite?)

;; FIXME: We should have this one in (yuni scheme)
(define (xinfinite? val)
  (cond
    ((finite? val) #f)
    ((nan? val) #f)
    (else #t)))

(define $fl-infinite? xinfinite?)
(define $fl= =)
(define $fl<= <=)
(define $fl>= >=)
(define $fl< <)
(define $fl> >)
(define $fl+ (pinexact +))
(define $fl- (pinexact -))
(define $fl* (pinexact *))
(define $fl/ (pinexact /))
(define $fl->fx exact)
(define $fl-expt (pinexact expt))
(define $fl-floor floor)
(define $fl-ceiling ceiling)
(define $fl-truncate truncate)
(define $fl-round round)
(define $fl-acos acos)
(define $fl-asin asin)
(define $fl-atan atan)
(define $fl-atan2 atan)
(define $fl-cos cos)
(define $fl-sin sin)
(define $fl-tan tan)
(define $fl-exp exp)
(define $fl-log log)
(define $fl-loge log)
(define $fl-sqrt sqrt)
(define $fl-floor/ (pinexact2 floor/))
(define $fl-truncate/ (pinexact2 truncate/))

(define $make-string make-string)
(define $make-bytevector make-bytevector)
(define ($make-vector len) (make-vector len #f))

(define $boolean=? boolean=?)
(define $char=? char=?)
(define $symbol=? symbol=?)

;; Filehandle
(define fh-stdin #f)
(define fh-stdout #f)
(define fh-stderr #f)

(define fh-vec #f)

(define (fh-alloc)
  (unless fh-vec
    (set! fh-vec (make-vector 10 #f)))
  (let ((len (vector-length fh-vec)))
   (let loop ((idx 0))
    (cond
      ((= idx len)
       (set! fh-vec (vector-append fh-vec (make-vector 5 #f)))
       idx)
      ((vector-ref fh-vec idx)
       (loop (+ idx 1)))
      (else
        idx)))))

(define (fh-free! idx)
  (vector-set! fh-vec idx #f))

(define (fh-ref fh)
  (vector-ref fh-vec fh))

(define (fh-set! obj)
  (let ((idx (fh-alloc)))
   (vector-set! fh-vec idx obj)
   idx))

(define (filehandle-init!)
  (let ((stdin (fh-set! (current-input-port)))
        (stdout (fh-set! (current-output-port)))
        (stderr (fh-set! (current-error-port))))
    (set! fh-stdin stdin)
    (set! fh-stdout stdout)
    (set! fh-stderr stderr)))

(define (filehandle-open/input filename)
  (fh-set! (open-binary-input-file filename)))

(define (filehandle-open/output filename)
  (fh-set! (open-binary-output-file filename)))

(define (filehandle-close fh)
  (close-port (fh-ref fh))
  (fh-free! fh))

(define (filehandle-read! fh bv offs len) ;; => len
  (let ((r (read-bytevector! bv (fh-ref fh) offs (+ offs len))))
   (if (eof-object? r) 0 r)))

(define (filehandle-write fh bv offs len)
  ;; FIXME: Always success???
  (write-bytevector bv (fh-ref fh) offs (+ offs len))  )

(define (filehandle-flush fh)
  (flush-output-port (fh-ref fh)))

(define (filehandle-stdin) fh-stdin)
(define (filehandle-stdout) fh-stdout)
(define (filehandle-stderr) fh-stderr)



(define compatlibs-proc-vector
  (vector
    ;; Non-standard core ops 
    $undefined
    $append
    $fx-length
    $fx=
    $fx<=
    $fx>=
    $fx<
    $fx>
    $fx+
    $fx-
    $fx*
    $fx/
    $fx->fl
    $fx-expt
    $fx-floor/
    $fx-truncate/
    $fl-nan?
    $fl-finite?
    $fl-infinite?
    $fl=
    $fl<=
    $fl>=
    $fl<
    $fl>
    $fl+
    $fl-
    $fl*
    $fl/
    $fl->fx
    $fl-expt
    $fl-floor
    $fl-ceiling
    $fl-truncate
    $fl-round
    $fl-acos
    $fl-asin
    $fl-atan
    $fl-atan2
    $fl-cos
    $fl-sin
    $fl-tan
    $fl-exp
    $fl-log
    $fl-loge
    $fl-sqrt
    $fl-floor/
    $fl-truncate/
    $make-string
    $make-bytevector
    $make-vector
    $boolean=?
    $char=?
    $symbol=?
    ;; Simple-struct
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
    ;; Filehandle
    filehandle-init!
    filehandle-open/input
    filehandle-open/output
    filehandle-close
    filehandle-read!
    filehandle-write
    filehandle-flush
    filehandle-stdin
    filehandle-stdout
    filehandle-stderr
    )
  )         

(define compatlibs-name-vector
  '#(
    ;; Non-standard core ops 
    $undefined
    $append
    $fx-length
    $fx=
    $fx<=
    $fx>=
    $fx<
    $fx>
    $fx+
    $fx-
    $fx*
    $fx/
    $fx->fl
    $fx-expt
    $fx-floor/
    $fx-truncate/
    $fl-nan?
    $fl-finite?
    $fl-infinite?
    $fl=
    $fl<=
    $fl>=
    $fl<
    $fl>
    $fl+
    $fl-
    $fl*
    $fl/
    $fl->fx
    $fl-expt
    $fl-floor
    $fl-ceiling
    $fl-truncate
    $fl-round
    $fl-acos
    $fl-asin
    $fl-atan
    $fl-atan2
    $fl-cos
    $fl-sin
    $fl-tan
    $fl-exp
    $fl-log
    $fl-loge
    $fl-sqrt
    $fl-floor/
    $fl-truncate/
    $make-string
    $make-bytevector
    $make-vector
    $boolean=?
    $char=?
    $symbol=?
    ;; Simple-struct
    make-simple-struct
    simple-struct-name
    simple-struct-ref
    simple-struct-set!
    simple-struct?
    ;; Filehandle
    filehandle-init!
    filehandle-open/input
    filehandle-open/output
    filehandle-close
    filehandle-read!
    filehandle-write
    filehandle-flush
    filehandle-stdin
    filehandle-stdout
    filehandle-stderr
     ))
         
         
)
