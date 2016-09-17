(library (r6rs-common-yuni compat bitwise primitives)
         (export
           bv-read/s8
           bv-read/u8
           bv-read/s16
           bv-read/u16
           bv-read/s32
           bv-read/u32
           bv-read/s64
           bv-read/u64
           bv-read/f32
           bv-read/f64
           bv-read/asciiz
           bv-write/s8!
           bv-write/u8!
           bv-write/s16!
           bv-write/u16!
           bv-write/s32!
           bv-write/u32!
           bv-write/s64!
           bv-write/u64!
           bv-write/f32!
           bv-write/f64!
           bv-write/asciiz!
           )
         (import 
           (only (yuni scheme) utf8->string)
           (except (rnrs) utf8->string))


(define (bv-read/s8 bv off) (bytevector-s8-ref bv off))
(define (bv-read/u8 bv off) (bytevector-u8-ref bv off))
(define (bv-read/s16 bv off) (bytevector-s16-native-ref bv off))
(define (bv-read/u16 bv off) (bytevector-u16-native-ref bv off))
(define (bv-read/s32 bv off) (bytevector-s32-native-ref bv off))
(define (bv-read/u32 bv off) (bytevector-u32-native-ref bv off))
(define (bv-read/s64 bv off) (bytevector-s64-native-ref bv off))
(define (bv-read/u64 bv off) (bytevector-u64-native-ref bv off))
(define (bv-read/f32 bv off) (bytevector-ieee-single-native-ref bv off))
(define (bv-read/f64 bv off) (bytevector-ieee-double-native-ref bv off))

(define (bv-read/asciiz bv off bufsiz)
  (define (step2-split s)
    (utf8->string bv off s))
  (define (step1-search-zero cur)
    (if (= 0 (bv-read/u8 bv cur))
      (step2-split (- cur 1))
      (step1-search-zero (+ cur 1))))
  (step1-search-zero off))

(define (bv-write/s8! bv off o) (bytevector-s8-set! bv off o))
(define (bv-write/u8! bv off o) (bytevector-u8-set! bv off o))
(define (bv-write/s16! bv off o) (bytevector-s16-native-set! bv off o))
(define (bv-write/u16! bv off o) (bytevector-u16-native-set! bv off o))
(define (bv-write/s32! bv off o) (bytevector-s32-native-set! bv off o))
(define (bv-write/u32! bv off o) (bytevector-u32-native-set! bv off o))
(define (bv-write/s64! bv off o) (bytevector-s64-native-set! bv off o))
(define (bv-write/u64! bv off o) (bytevector-u64-native-set! bv off o))
(define (bv-write/f32! bv off o) (bytevector-ieee-single-native-set!
                                   bv off o))
(define (bv-write/f64! bv off o) (bytevector-ieee-double-native-set!
                                   bv off o))

(define (bv-write/asciiz! bv off bufsiz str)
  (define gen (string->utf8 str))
  (define genlen (bytevector-length gen))
  (define zeropoint (+ off genlen))
  (bytevector-copy! bv off gen 0 genlen)
  (bv-write/u8! bv zeropoint 0))

)
