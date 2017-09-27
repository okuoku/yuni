(library (sagittarius-yuni compat ffi primitives)
         (export yuniffi-nccc-call
                 yuniffi-nccc-ptr->callable
                 yuniffi-module-load
                 yuniffi-module-lookup
                 
                 ;; Memory OPs (pointers)
                 ptr? 
                 ptr-read/w64ptr
                 ptr-write/w64ptr!
                 ptr-read/s8 ptr-read/u8 ptr-read/s16 ptr-read/u16
                 ptr-read/s32 ptr-read/u32 ptr-read/s64 ptr-read/u64
                 ptr-read/asciiz
                 ptr-write/s8! ptr-write/u8! ptr-write/s16! ptr-write/u16!
                 ptr-write/s32! ptr-write/u32! ptr-write/s64! ptr-write/u64!
                 ptr-write/asciiz!
                 
                 bv-read/w64ptr
                 bv-write/w64ptr!)
         (import (yuni scheme)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (yuni compat bitwise primitives)
                 (sagittarius ffi)
                 (only (sagittarius) load-path))
         
(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (func (address in  (* 8  in-offset)) in-size 
        (address out (* 8 out-offset)) out-size))

;; Mostly same as nmosh

(define (ptr? x) (pointer? x))

(define (ptr-read/s8 x off) (pointer-ref-c-int8 x off))
(define (ptr-read/u8 x off) (pointer-ref-c-uint8 x off))
(define (ptr-read/s16 x off) (pointer-ref-c-int16 x off))
(define (ptr-read/u16 x off) (pointer-ref-c-uint16 x off))
(define (ptr-read/s32 x off) (pointer-ref-c-int32 x off))
(define (ptr-read/u32 x off) (pointer-ref-c-uint32 x off))
(define (ptr-read/s64 x off) (pointer-ref-c-int64 x off))
(define (ptr-read/u64 x off) (pointer-ref-c-uint64 x off))
(define (ptr-write/s8! x off v) (pointer-set-c-int8! x off v))
(define (ptr-write/u8! x off v) (pointer-set-c-uint8! x off v))
(define (ptr-write/s16! x off v) (pointer-set-c-int16! x off v))
(define (ptr-write/u16! x off v) (pointer-set-c-uint16! x off v))
(define (ptr-write/s32! x off v) (pointer-set-c-int32! x off v))
(define (ptr-write/u32! x off v) (pointer-set-c-uint32! x off v))
(define (ptr-write/s64! x off v) (pointer-set-c-int64! x off v))
(define (ptr-write/u64! x off v) (pointer-set-c-uint64! x off v))

(define (ptr-read/w64ptr x off)
  (integer->pointer (ptr-read/u64 x off)))
(define (ptr-write/w64ptr! x off v)
  (ptr-write/u64 x off (pointer->integer v)))

(define (bv-read/w64ptr x off)
  (integer->pointer (bv-read/u64 x off)))
(define (bv-write/w64ptr! x off v)
  (bv-write/u64! x off (pointer->integer v)))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define (module-load path) ;; => pointer / #f
  (let ((handle (open-shared-library path)))
   (and (not (null-pointer? handle))
        handle)))
         
(define (yuniffi-module-lookup handle str)
  (define ptr (lookup-shared-library handle str))
  (pointer->c-function ptr 'void str '(void* int void* int)))

(define (yuniffi-nccc-ptr->callable ptr)
  (pointer->c-function ptr 'void 'nccc-func '(void* int void* int)))

(define (module-path) (load-path))

(define yuniffi-module-load (make-simpleloader module-path module-load))

)
