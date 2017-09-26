(library (chibi-scheme-yuni compat ffi primitives)
         (export
           yuniffi-nccc-call
           yuniffi-nccc-ptr->callable
           yuniffi-nccc-proc-register
           yuniffi-nccc-proc-release
           yuniffi-module-load
           yuniffi-module-lookup
           yuniffi-callback-helper

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
                 (yuni ffi runtime bootstraploader)
                 (yuni ffi runtime simpleloader)
                 (yuni ffi runtime simplestrings)
                 (srfi 33)
                 ;; FIXME: Undocumented?
                 (only (chibi) current-module-path)
                 ;; FIXME: Rename this?
                 (yuniffi-runtime))

;;

;; FIXME: Assumes little endian
(define (ptr? x) (eq? 1 (yuniffi_pointerp x)))
(define (integer->ptr x) (yuniffi_offset_ptr #f x))
(define (ptr-read/s8 p off) (yuniffi_fetch_s8 p off))
(define (ptr-read/u8 p off) (yuniffi_fetch_u8 p off))
(define (ptr-read/s16 p off) (yuniffi_fetch_s16 p off))
(define (ptr-read/u16 p off) (yuniffi_fetch_u16 p off))
(define (ptr-read/s32 p off) (yuniffi_fetch_s32 p off))
(define (ptr-read/u32 p off) (yuniffi_fetch_u32 p off))
(define (ptr-write/s8! p off v) (yuniffi_store_s8 p off v))
(define (ptr-write/u8! p off v) (yuniffi_store_u8 p off v))
(define (ptr-write/s16! p off v) (yuniffi_store_s16 p off v))
(define (ptr-write/u16! p off v) (yuniffi_store_u16 p off v))
(define (ptr-write/s32! p off v) (yuniffi_store_s32 p off v))
(define (ptr-write/u32! p off v) (yuniffi_store_u32 p off v))

(define-read-asciiz ptr-read/asciiz ptr-read/u8)
(define-write-asciiz ptr-write/asciiz! ptr-write/u8!)

(define (ptr-read/w64ptr p off) (yuniffi_fetch_p64 p off))
(define (ptr-write/s64ptr! p off v) (yuniffi_store_p64 p off v))
(define (bv-read/w64ptr bv off) (yuniffi_fetch_p64_bv bv off))
(define (bv-write/w64ptr! bv off v) (yuniffi_store_p64_bv bv off v))

;; Implement 64bit variants
(define (ptr-read/s64 p off)
  (let ((lo (ptr-read/s32 p off))
        (hi (ptr-read/s32 p (+ 4 off))))
    (bitwise-ior (arithmetic-shift hi 32)
                lo)))
(define (ptr-read/u64 p off)
  (let ((lo (ptr-read/s32 p off))
        (hi (ptr-read/s32 p (+ 4 off))))
    (+ (arithmetic-shift hi 32)
       lo)))

(define (ptr-write/s64! p off v)
  (let ((lo (bitwise-and #xffffffff v))
        (hi (bitwise-and #xffffffff
                         (arithmetic-shift v -32))))
    (ptr-write/u32! p off lo)
    (ptr-write/u32! p (+ 4 off) hi)))

(define ptr-write/u64! ptr-write/s64!)

;;

(define (yuniffi-nccc-call func
                           in in-offset in-size
                           out out-offset out-size)
  (yuniffi_nccc_call func in in-offset in-size out out-offset out-size))

(define (yuniffi-nccc-proc-register proc)
  (yuniffi_nccc_proc_register proc))

(define (yuniffi-nccc-proc-release obj)
  (yuniffi_nccc_proc_release obj))

(define (yuniffi-callback-helper) (yuniffi_nccc_get_callback_bridge))

(define-values (dlopen dlsym) (make-bootstraploader yuniffi-nccc-call
                                                    yuniffi_nccc_bootstrap
                                                    ptr?
                                                    bv-read/w64ptr
                                                    bv-write/w64ptr!
                                                    ptr-write/asciiz!))

(define (module-load path) (dlopen path))
(define (yuniffi-module-lookup handle str) (dlsym handle str))

(define (module-path) (current-module-path))

(define yuniffi-module-load (make-simpleloader module-path module-load))

(define (yuniffi-nccc-ptr->callable ptr) ptr)
         
)
