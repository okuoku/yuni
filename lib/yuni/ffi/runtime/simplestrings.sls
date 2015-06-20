(library (yuni ffi runtime simplestrings)
         (export define-write-asciiz
                 define-read-asciiz)
         (import (yuni scheme))
         
(define-syntax define-write-asciiz
  (syntax-rules ()
    ((_ name w8)
     (define (name ptr off bufsiz str)
       (letrec* ((gen (string->utf8 str))
                 (genlen (bytevector-length gen))
                 (zeropoint (+ off genlen))
                 (loop (lambda (cur) 
                         (w8 ptr (+ off cur) (bytevector-u8-ref gen cur))
                         (unless (= cur genlen)
                           (loop (+ cur 1))))))
         (when (and (not (= -1 bufsiz)) (<= bufsiz genlen))
           (error "insufficient buffer size" bufsiz))
         (loop 0)
         (w8 ptr zeropoint 0))))))

(define-syntax define-read-asciiz
  (syntax-rules ()
    ((_ name r8)
     (define (name ptr off bufsiz)
       (letrec* ((bufend (+ bufsiz off))
                 (step1-search-zero (lambda (cur)
                                      (when (and (not (= bufsiz -1)) 
                                                 (>= cur bufend))
                                        (error "insufficient buffer size"
                                               bufsiz))
                                      (if (= 0 (r8 ptr cur))
                                        (step2-capture
                                          0
                                          cur
                                          (make-bytevector (- cur off)))
                                        (step1-search-zero (+ cur 1)))))
                 (step2-capture (lambda (cur end bv)
                                  (let ((o (+ cur off)))
                                   (if (= o end)
                                     (step3-output bv)
                                     (begin
                                       (bytevector-u8-set! bv cur (r8 ptr cur))
                                       (step2-capture (+ 1 cur) end bv))))))
                 (step3-output (lambda (bv) (utf8->string bv))))
         (step1-search-zero off))))))
)
