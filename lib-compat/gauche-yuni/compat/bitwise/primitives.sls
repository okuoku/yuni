(library (gauche-yuni compat bitwise primitives)
         (export
           bv-read/s8 bv-read/u8 bv-read/s16 bv-read/u16
           bv-read/s32 bv-read/u32 bv-read/s64 bv-read/u64
           bv-read/f32 bv-read/f64
           bv-read/asciiz
           bv-write/s8!  bv-write/u8!  bv-write/s16!  bv-write/u16!
           bv-write/s32! bv-write/u32!  bv-write/s64!  bv-write/u64!
           bv-write/f32! bv-write/f64!
           bv-write/asciiz!)
         (import (yuni scheme) (gauche uvector))
         

;; FIXME: Think more efficient primitive

;; R7RS
(define (bv-read/u8 bv o) (bytevector-u8-ref bv o))
(define (bv-write/u8! bv o) (bytevector-u8-set! bv o))

(define-syntax defread
  (syntax-rules ()
    ((_ name cls ref)
     (define (name bv o)
       (let ((uv (uvector-alias cls bv o)))
        (ref uv 0))))))

(defread bv-read/s8 <s8vector> s8vector-ref)
(defread bv-read/s16 <s16vector> s16vector-ref)
(defread bv-read/s32 <s32vector> s32vector-ref)
(defread bv-read/s64 <s64vector> s64vector-ref)
(defread bv-read/u16 <u16vector> u16vector-ref)
(defread bv-read/u32 <u32vector> u32vector-ref)
(defread bv-read/u64 <u64vector> u64vector-ref)
(defread bv-read/f32 <f32vector> f64vector-ref)
(defread bv-read/f64 <f64vector> f64vector-ref)

(define-syntax defwrite
  (syntax-rules ()
    ((_ name cls set)
     (define (name bv o v)
       (let ((uv (uvector-alias cls bv o)))
        (set uv 0 v))))))

(defwrite bv-write/s8! <s8vector> s8vector-set!)
(defwrite bv-write/s16! <s16vector> s16vector-set!)
(defwrite bv-write/s32! <s32vector> s32vector-set!)
(defwrite bv-write/s64! <s64vector> s64vector-set!)
(defwrite bv-write/u16! <s16vector> u16vector-set!)
(defwrite bv-write/u32! <u32vector> u32vector-set!)
(defwrite bv-write/u64! <u64vector> u64vector-set!)
(defwrite bv-write/f32! <f32vector> f32vector-set!)
(defwrite bv-write/f64! <f64vector> f64vector-set!)

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
