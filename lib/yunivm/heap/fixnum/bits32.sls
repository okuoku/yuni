(library (yunivm heap fixnum bits32)
         (export bitset-s32?
                 bcl-s32
                 bts-s32
                 bsr-s32)
         (import (yuni scheme))

         
;;
(define (bitset-s32? val pos)
  (cond
    ((= pos 31) (negative? val))
    (else 
      ;; Bit reverse if val was negative
      (if (negative? val)
        (even? (quotient (- (- val) 1) (expt 2 pos)))
        (odd? (quotient val (expt 2 pos)))))))

(define (bcl-s32 val pos)
  (cond
    ((= pos 31)
     (cond
       ((negative? val)
        (+ val 2147483648))
       (else val)))
    (else
      (let ((e (expt 2 pos)))
       ;; Subtract expt if the bit set
       (if (negative? val)
         (if (even? (quotient (- (- val) 1) e))
           (- val e)
           val)
         (if (odd? (quotient val e))
           (- val e)
           val))))))

(define (bts-s32 val pos)
  (cond
    ((= pos 31)
     (cond
       ((negative? val)
        val)
       (else
         (- (+ 1 (- 2147483647 val))))))
    (else
      (let ((e (expt 2 pos)))
       ;; Add expt if the bit not set
       (if (negative? val)
         (if (odd? (quotient (- (- val) 1) e))
           (+ val e)
           val)
         (if (even? (quotient val e))
           (+ val e)
           val))))))

(define (bsr-s32 val) ;; => idx, #f
  ;; FIXME: We can reduce branches by utilizing tables...
  (cond
    ((= 0 val) #f)
    ((> 0 val) 31)
    ((= 1 val) 0)
    ((<= 1073741824 val) 30)
    ((<= 536870912 val) 29)
    ((<= 268435456 val) 28)
    ((<= 134217728 val) 27)
    ((<= 67108864 val) 26)
    ((<= 33554432 val) 25)
    ((<= 16777216 val) 24)
    ((<= 8388608 val) 23)
    ((<= 4194304 val) 22)
    ((<= 2097152 val) 21)
    ((<= 1048576 val) 20)
    ((<= 524288 val) 19)
    ((<= 262144 val) 18)
    ((<= 131072 val) 17)
    ((<= 65536 val) 16)
    ((<= 32768 val) 15)
    ((<= 16384 val) 14)
    ((<= 8192 val) 13)
    ((<= 4096 val) 12)
    ((<= 2048 val) 11)
    ((<= 1024 val) 10)
    ((<= 512 val) 9)
    ((<= 256 val) 8)
    ((<= 128 val) 7)
    ((<= 64 val) 6)
    ((<= 32 val) 5)
    ((<= 16 val) 4)
    ((<= 8 val) 3)
    ((<= 4 val) 2)
    ((<= 2 val) 1)
    ;; NaN
    (else "Huh?")))

         
)
