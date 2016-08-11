(library (picrin-yuni compat bitwise primitives)
         (export
           ;; Memory OPs (bytevectors)
           bv-read/s8 bv-read/u8 bv-read/s16 bv-read/u16
           bv-read/s32 bv-read/u32 bv-read/s64 bv-read/u64
           bv-read/f32 bv-read/f64
           bv-read/asciiz
           bv-write/s8!  bv-write/u8!  bv-write/s16!  bv-write/u16!
           bv-write/s32! bv-write/u32!  bv-write/s64!  bv-write/u64!
           bv-write/f32! bv-write/f64!
           bv-write/asciiz!)
         (import (yuni scheme) (srfi 60))


(define (bytevector-u16-ref-le bv o)
  (let ((hi (bytevector-u8-ref bv (+ o 1)))
        (lo (bytevector-u8-ref bv o)))
    (+ (arithmetic-shift hi 8)
       lo)))

(define (bytevector-u32-ref-le bv o)
  (let ((hi (bytevector-u16-ref-le bv (+ o 2)))
        (lo (bytevector-u16-ref-le bv o)))
    (+ (arithmetic-shift hi 16)
       lo)))

(define s8bits (bitwise-not 255))
(define s16bits (bitwise-not 65535))

(define (bytevector-s8-ref bv o)
  (let ((b (bytevector-u8-ref bv o)))
   (if (> b 127)
     (bitwise-ior s8bits b)
     b)))
(define (bytevector-s16-ref-le bv o)
  (let ((b (bytevector-u16-ref-le bv o)))
   (if (> b 32767)
     (bitwise-ior s16bits b)
     b)))

;; Since Picrin does not have bignums, we have to a bit serious here.

(define (bytevector-u16-ref-le/bf bv o)
  (let ((b (bytevector-u16-ref-le bv o)))
   (bitwise-xor 65535 b)))

(define (bytevector-u32-ref-le/bf bv o)
  (let ((lo (bytevector-u16-ref-le/bf bv o))
        (hi (bytevector-u16-ref-le/bf bv (+ o 2))))
    (+ (arithmetic-shift hi 16)
       lo)))

(define (bytevector-s32-ref-le bv o)
  ;; Check MSB first
  (let ((b (bytevector-u8-ref bv (+ o 3))))
   (if (> b 127)
     (exact (- 0 (bytevector-u32-ref-le/bf bv o) 1))
     (exact (bytevector-u32-ref-le bv o)))))

(define (bytevector-s8-set! bv o v)
  (define b (bitwise-and 255 v))
  (bytevector-u8-set! bv o b))

(define (bv-read/s8 bv o) (bytevector-s8-ref bv o))
(define (bv-read/u8 bv o) (bytevector-u8-ref bv o))
(define (bv-read/s16 bv o) (bytevector-s16-ref-le bv o))
(define (bv-read/u16 bv o) (bytevector-u16-ref-le bv o))
(define (bv-read/s32 bv o) (bytevector-s32-ref-le bv o))
(define (bv-read/u32 bv o) (exact (bytevector-u32-ref-le bv o)))

(define (bv-read/f32 bv o)
  (display "WARNING: NOTIMPLEMENTED f32")(newline)
  0.0)
(define (bv-read/f64 bv o)
  (display "WARNING: NOTIMPLEMENTED f64")(newline)
  0.0)
(define (bv-write/f32! bv o v)
  (display "WARNING: NOTIMPLEMENTED f32")(newline))
(define (bv-write/f64! bv o v)
  (display "WARNING: NOTIMPLEMENTED f64")(newline))

(define (bv-read/u64 bv o)
  (let ((hi (bv-read/u32 bv (+ o 4)))
        (lo (bv-read/u32 bv o)))
    (+ (arithmetic-shift hi 32)
       lo)))

(define (bytevector-s64-ref-le/bf bv o)
  (let ((lo (bytevector-u32-ref-le/bf bv o))
        (hi (bytevector-u32-ref-le/bf bv (+ o 4))))
    (+ (arithmetic-shift hi 32)
       lo)))

(define (bv-read/s64 bv o)
  ;; Check MSB first
  (let ((b (bytevector-u8-ref bv (+ o 7))))
   (if (> b 127)
     (exact (- 0 (bytevector-s64-ref-le/bf bv o) 1))
     (exact (bv-read/u64 bv o)))))

(define (bv-write/s8! bv o v) (bytevector-s8-set! bv o v))
(define (bv-write/u8! bv o v) (bytevector-u8-set! bv o v))

;; Chibi does not provide any multi-byte write primitives.
;; So we need some shift registers here..

(define-syntax defwrt
  (syntax-rules ()
    ((_ nam width)
     (define (nam bv o v)
       (let* ((bitflip? (negative? v))
              (start (if bitflip? (- 0 (+ 1 v)) v)))
         (letrec ((loop (lambda (p reg)
                          (unless (= p width)
                            (let ((b (bitwise-and 255 reg))
                                  (next (arithmetic-shift reg -8)))
                              (bytevector-u8-set! 
                                bv (+ o p) 
                                (if bitflip?
                                  (bitwise-xor 255 b)
                                  b))
                              (loop (+ 1 p) next))))))
           (loop 0 start)))))))

(defwrt bv-write/s16! 2)
(defwrt bv-write/u16! 2)
(defwrt bv-write/s32! 4)
(defwrt bv-write/u32! 4)
(defwrt bv-write/s64! 8)
(defwrt bv-write/u64! 8)

;; Copied from (yuni-r6rs ffi memory)
(define (bv-read/asciiz bv off bufsiz)
  (define (step2-split s)
    (utf8->string bv off s))
  (define (step1-search-zero cur)
    (if (= 0 (bv-read/u8 bv cur))
      (step2-split (- cur 1))
      (step1-search-zero (+ cur 1))))
  (step1-search-zero off))

(define (bv-write/asciiz! bv off bufsiz str)
  (define gen (string->utf8 str))
  (define genlen (bytevector-length gen))
  (define zeropoint (+ off genlen))
  (bytevector-copy! bv off gen 0 genlen)
  (bv-write/u8! bv zeropoint 0))

)
