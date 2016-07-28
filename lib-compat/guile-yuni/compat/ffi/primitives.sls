(library (guile-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-module-load
                 yuniffi-module-lookup

                 ;; Memory OPs (pointers)
                 ptr?
                 ptr-read/w64ptr
                 ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
                 ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
                 ptr-read/asciiz
                 ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
                 ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
                 ptr-write/asciiz!

                 bv-read/w64ptr
                 bv-write/w64ptr!
                 )
         (import (yuni scheme)
                 (only (guile)
                       %load-path
                       dynamic-link
                       dynamic-func)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives)
                 (system foreign))

;; Guile requires bytevector to do byte-wise access. Whoa.

(define (ptr? x) (pointer? x))
(define-syntax defreader
  (syntax-rules ()
    ((_ name bvr)
     (define (name p off)
       (bvr (pointer->bytevector p 8 off) 0)))))

(defreader ptr-read/s8 bv-read/s8)
(defreader ptr-read/u8 bv-read/u8)
(defreader ptr-read/s16 bv-read/s16)
(defreader ptr-read/u16 bv-read/u16)
(defreader ptr-read/s32 bv-read/s32)
(defreader ptr-read/u32 bv-read/u32)
(defreader ptr-read/s64 bv-read/s64)
(defreader ptr-read/u64 bv-read/u64)

(define-syntax defwriter
  (syntax-rules ()
    ((_ name bvw)
     (define (name p off o)
       (bvw (pointer->bytevector p 8 off) 0 o)))))

(defwriter ptr-write/s8! bv-write/s8!)
(defwriter ptr-write/u8! bv-write/u8!)
(defwriter ptr-write/s16! bv-write/s16!)
(defwriter ptr-write/u16! bv-write/u16!)
(defwriter ptr-write/s32! bv-write/s32!)
(defwriter ptr-write/u32! bv-write/u32!)
(defwriter ptr-write/s64! bv-write/s64!)
(defwriter ptr-write/u64! bv-write/u64!)

(define (ptr-read/w64ptr p off)
  (let ((w (ptr-read/u64 p off)))
   (make-pointer w)))
(define (ptr-write/w64ptr! p off v)
  (ptr-write/u64! p off (pointer-address v)))
(define (bv-read/w64ptr bv off)
  (let ((w (bv-read/u64 bv off)))
   (make-pointer w)))
(define (bv-write/w64ptr bv off v)
  (bv-write/u64! bv off (pointer-address v)))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (let ((inp (bytevector->pointer in (* 8 in-offset)))
        (outp (bytevector->pointer out (* 8 out-offset))))
    (func inp in-size outp out-size)))

(define (module-path) %load-path)

(define (module-load path)
  (guard (c (#t #f))
         (dynamic-link path)))

(define yuniffi-module-load (make-simpleloader module-path module-load))
         
(define (yuniffi-module-lookup handle str)
  (define p (dynamic-func str handle))
  (pointer->procedure void p (list '* int '* int)))
         
)
